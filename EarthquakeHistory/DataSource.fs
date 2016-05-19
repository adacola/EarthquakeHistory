namespace Adacola.EarthquakeHistory.DataSource

type DataSource = DataSource of string

open System
open System.Net
open System.Web
open System.Text.RegularExpressions
open FParsec.Primitives
open FParsec.CharParsers
open Adacola.EarthquakeHistory.Model

module Wikipedia =
    let private readFrom uri = async {
        use client = new WebClient(Encoding = Text.Encoding.UTF8)
        return! client.AsyncDownloadString uri
    }

    let internal removeUnwantedText page =
        (page, [@"<a .*?>"; @"</a>"; @"<span .*?>"; @"</span>"; @"<b>"; @"</b>"; @"<sup .*?>.*?</sup>"; @"<sup>"; @"</sup>"; @"<sub>"; @"</sub>"]) ||> Seq.fold (fun acc pattern -> Regex.Replace(acc, pattern, "", RegexOptions.Singleline))

    let internal getEarthquakeListString page =
        match Regex.Match(page, @"<h2>416年〜鎌倉時代(?<earthquakeList>.*?)<h2>脚注", RegexOptions.Singleline) with
        | m when m.Success -> m.Groups.["earthquakeList"].Value
        | _ -> invalidArg "page" "地震リストの箇所が見つかりません"

    /// 地震リスト部分をパーズします。
    /// 現在、以下の内容のリスト行は無視します。
    /// * 日まで指定されていない場合。
    /// * 何日～何日 のように範囲指定されている場合。
    /// * マグニチュードや震度が範囲指定や不等号指定されている場合。
    let internal parsePage page =
        let tagStart tag = attempt (pchar '<' >>. pstringCI tag >>. (skipChar '>' <|> skipString "/>" <|> (spaces1 >>. skipManyTill anyChar (pchar '>'))))
        let tagEnd tag = attempt (pstring "</" >>. pstringCI tag >>. skipChar '>')
        let parenStart = pchar '（'
        let parenEnd = pchar '）'
        let paren parser = attempt (between parenStart parenEnd parser)
        let parenMany1Chars = paren (many1Chars (noneOf ['）']))
        let pint postfix = attempt (many1Chars digit .>> postfix) |>> int
        /// parserが成功した場合はUserStateをparserの結果の値に更新。parserの結果が全体のパーズ結果となります
        let tapUserState parser = parser >>= (fun x -> setUserState x >>% x)

        let tagLiStart = tagStart "li"
        let tagLiEnd = tagEnd "li"
        let tagUlStart = tagStart "ul"
        let tagUlEnd = tagEnd "ul"
        let skipToTagLiStart = skipManyTill anyChar tagLiStart
        let earthquakeListElementEnd = attempt (tagLiEnd <|> (spaces >>. tagUlStart))

        let year = pint (pchar '年')
        let month = pint (pchar '月')
        let day = pint (pchar '日')
        let sameDate = pstring "同日" >>. getUserState
        let simpleDate =
            sameDate <|> pipe4 getUserState (opt year) (opt month) day (fun (dt : DateTime) y m d -> DateTime(defaultArg y dt.Year, defaultArg m dt.Month, d))
            |> tapUserState |> attempt
        /// ユリウス暦（グレゴリオ暦）（和暦） をパーズしてグレゴリオ暦を取得
        let juliusJapaneseDate = attempt (simpleDate >>. paren simpleDate .>> parenMany1Chars)
        /// グレゴリオ暦（和暦） をパーズしてグレゴリオ暦を取得
        let japaneseDate = attempt (simpleDate .>> parenMany1Chars)
        /// 年のみの日付のパターンをパーズして年をUserStateに設定
        let yearOnly = attempt (year .>> (optional parenMany1Chars) >>= (fun y -> DateTime(y, 1, 1) |> setUserState))
        /// 年（和暦）月日 をパーズしてグレゴリオ暦を取得
        let japaneseYearAndMonthDay = attempt (yearOnly >>. simpleDate)
        /// 年のみの日付でリストが終了するパターンをパーズして年をUserStateに設定
        let yearOnlyAndElementEnd = attempt (yearOnly .>> skipManyTill anyChar earthquakeListElementEnd)
        /// 日付のパターンをパーズしてグレゴリオ暦を取得
        let dateWithoutYearOnly = attempt (juliusJapaneseDate <|> japaneseDate <|> japaneseYearAndMonthDay <|> simpleDate)

        let hour = pint (pchar '時')
        let minute = pint (pchar '分')
        let time =
            parse {
                let! hour, minute = attempt (hour .>>. minute .>> (pchar '頃'))
                let! (date : DateTime) = getUserState
                return DateTime(date.Year, date.Month, date.Day, hour, minute, 0)
            } |> tapUserState |> attempt
        let dateTimeWithoutYearOnly = attempt (pipe2 dateWithoutYearOnly (opt time) (fun d t -> t |> defaultArg <| d, Option.isSome t))

        let nameSep = attempt (spaces1 >>. optional (pchar '※' >>. spaces1) >>. pchar '-' >>. spaces1)
        let nameEnd parser = attempt (followedBy parser <|> earthquakeListElementEnd) >>. parser
        /// ～で地震 のような名前のない地震をパーズ
        let regionName = attempt (spaces >>. many1CharsTill anyChar (nameEnd (pstring "で地震")) .>> nameSep |>> (Region >> Some))
        /// ～地震（～大震災） のような別名ありの地震名をパーズ
        let earthquakeNameAndAlternativeName =
            spaces >>. pipe2 (many1CharsTill anyChar (followedBy parenStart)) parenMany1Chars (fun n an -> Some(EarthquakeName {Name = n; AlternativeName = Some an})) .>> nameSep
            |> attempt
        /// ～地震 のような別名なしの地震名をパーズ
        let earthquakeName = attempt (spaces >>. many1CharsTill anyChar (nameEnd nameSep) |>> fun n -> Some(EarthquakeName {Name = n; AlternativeName = None}))
        let noName = attempt (skipManyTill anyChar (nameEnd nameSep)) <|> spaces >>% None
        /// すべての地震名のパターンをパーズして地震名を取得
        let name = regionName <|> earthquakeNameAndAlternativeName <|> earthquakeName <|> noName

        let info prefix parser = attempt (skipManyTill anyChar (followedBy prefix <|> followedBy earthquakeListElementEnd) >>. prefix >>. spaces >>. parser)
        let seismicIntensity = info (skipString "震度") (pint32 .>>. opt ((pstring "弱" >>% false) <|> (pstring "強" >>% true)))
        let magnitude = info (skipString "Mj" <|> skipChar 'M') pfloat
        let momentMagnitude = info (skipString "Mw") pfloat
        let detail = manyCharsTill anyChar earthquakeListElementEnd
        /// 震度、マグニチュード、モーメントマグニチュード、詳細情報をパーズして取得
        let infos = tuple4 (opt (lookAhead seismicIntensity)) (opt (lookAhead magnitude)) (opt (lookAhead momentMagnitude)) detail

        let earthquake =
            let toEarthquake (dateTime, includesTime) name (intensity, magnitude, momentMagnitude, detail) =
                {
                    OccuredDate = {DateTime = dateTime; IncludesTime = includesTime}
                    Name = name
                    SeismicIntensity = intensity |> Option.map (fun (i, u) -> SeismicIntensity.Create(dateTime, i, u))
                    Magnitude = magnitude
                    MomentMagnitude = momentMagnitude
                    Detail = detail
                }
            attempt (pipe3 dateTimeWithoutYearOnly name infos toEarthquake)

        let earthquakeListElement = skipToTagLiStart >>. ((earthquake |>> Some) <|> (yearOnlyAndElementEnd >>% None) <|>% None)
        let earthquakeListEnd = attempt (skipMany (spaces >>. (tagUlEnd <|> tagLiEnd)) .>> spaces .>> eof)
        let earthquakeList = manyTill earthquakeListElement earthquakeListEnd |>> List.choose id

        // パーズの実行
        match runParserOnString earthquakeList DateTime.MinValue "page" page with
        | Success(result, _, _) -> result
        | Failure(error, _, _) as f -> failwithf "ページのパーズに失敗しました。\n%s" error 

    let parseFrom uri = async {
        let! page = readFrom uri
        let earthquakeListString = page |> removeUnwantedText |> HttpUtility.HtmlDecode |> getEarthquakeListString
        return parsePage earthquakeListString
    }

    let parse() =
        let earthquakeListPageUri = Uri("https://ja.wikipedia.org/wiki/%E5%9C%B0%E9%9C%87%E3%81%AE%E5%B9%B4%E8%A1%A8_(%E6%97%A5%E6%9C%AC)")
        parseFrom earthquakeListPageUri
