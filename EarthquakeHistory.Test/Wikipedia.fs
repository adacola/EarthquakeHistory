namespace Adacola.EarthquakeHistory.Test

open NUnit.Framework
open FsUnit
open FsCheck

open Adacola.EarthquakeHistory.DataSource
open Adacola.EarthquakeHistory.Model
open System


[<TestFixture>]
module Wikipedia =

    let notNullStringGen = Arb.generate<string> |> Gen.suchThat (not << isNull)
    let tagAttributeGen = notNullStringGen |> Gen.suchThat (fun x -> not (x.Contains "<" || x.Contains ">"))
    let notTagGen = notNullStringGen |> Gen.suchThat (fun x -> not (x.Contains("<") || x.Contains("</")))

    let testRemoveTagWithinAttribute tag =
        let arb =
            gen {
                let! prefix = notTagGen
                let! tagAttribute = tagAttributeGen
                let! tagInner = notTagGen
                let! postfix = notTagGen
                return prefix, tagAttribute, tagInner, postfix
            } |> Arb.fromGen
        Prop.forAll arb (fun (prefix, tagAttribute, tagInner, postfix) ->
            let source = sprintf "%s<%s %s>%s</%s>%s" prefix tag tagAttribute tagInner tag postfix
            let expected = sprintf "%s%s%s" prefix tagInner postfix
            Wikipedia.removeUnwantedText source = expected)
        |> Check.QuickThrowOnFailure
    
    let testRemoveTagWithoutAttribute tag =
        let arb =
            gen {
                let! prefix = notTagGen
                let! tagInner = notTagGen
                let! postfix = notTagGen
                return prefix, tagInner, postfix
            } |> Arb.fromGen
        Prop.forAll arb (fun (prefix, tagInner, postfix) ->
            let source = sprintf "%s<%s>%s</%s>%s" prefix tag tagInner tag postfix
            let expected = sprintf "%s%s%s" prefix tagInner postfix
            Wikipedia.removeUnwantedText source = expected)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``removeUnwantedTextが<a>を削除すること`` () =
        testRemoveTagWithinAttribute "a"

    [<Test>]
    let ``removeUnwantedTextが<span>を削除すること`` () =
        testRemoveTagWithinAttribute "span"

    [<Test>]
    let ``removeUnwantedTextが<b>を削除すること`` () =
        testRemoveTagWithoutAttribute "b"

    [<Test>]
    let ``removeUnwantedTextが属性付き<sup>をタグ内部ごと削除すること`` () =
        let arb =
            gen {
                let! prefix = notTagGen
                let! tagAttribute = tagAttributeGen
                let! tagInner = notTagGen
                let! postfix = notTagGen
                return prefix, tagAttribute, tagInner, postfix
            } |> Arb.fromGen
        Prop.forAll arb (fun (prefix, tagAttribute, tagInner, postfix) ->
            let source = sprintf "%s<sup %s>%s</sup>%s" prefix tagAttribute tagInner postfix
            let expected = sprintf "%s%s" prefix postfix
            Wikipedia.removeUnwantedText source = expected)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``removeUnwantedTextが属性なし<sup>をタグのみ削除すること`` () =
        testRemoveTagWithoutAttribute "sup"

    [<Test>]
    let ``removeUnwantedTextが<sub>を削除すること`` () =
        testRemoveTagWithoutAttribute "sub"

    let excludedTagGen =
        Arb.generate<string> |> Gen.suchThat (fun x -> not (isNull x || x = "a" || x = "span" || x = "b" || x = "sup" || x = "sub" || x.Contains "<" || x.Contains ">"))

    [<Test>]
    let ``removeUnwantedTextが<a>,<span>,<b>,<sup>以外を削除しないこと`` () =
        let arb =
            gen {
                let! prefix = notTagGen
                let! tag = excludedTagGen
                let! tagAttribute = Gen.oneof [tagAttributeGen |> Gen.map ((+) " "); Gen.constant ""]
                let! tagInner = notTagGen
                let! postfix = notTagGen
                return sprintf "%s<%s%s>%s</%s>%s" prefix tag tagAttribute tagInner tag postfix
            } |> Arb.fromGen
        Prop.forAll arb (fun source -> Wikipedia.removeUnwantedText source = source)
        |> Check.QuickThrowOnFailure
        
    [<Test>]
    let ``getEarthquakeListStringで地震リストの範囲の文字列を取得できること`` () =
        let startString = "<h2>416年〜鎌倉時代"
        let endString = "<h2>脚注"
        let notStartGen = notNullStringGen |> Gen.suchThat (fun x -> not (x.Contains startString))
        let notEndGen = notNullStringGen |> Gen.suchThat (fun x -> not (x.Contains endString))
        let arb =
            gen {
                let! prefix = notStartGen
                let! inner = notEndGen
                let! postfix = notNullStringGen
                return prefix, inner, postfix
            } |> Arb.fromGen
        Prop.forAll arb (fun (prefix, inner, postfix) ->
            let source = sprintf "%s%s%s%s%s" prefix startString inner endString postfix
            Wikipedia.getEarthquakeListString source = inner)
        |> Check.QuickThrowOnFailure

[<TestFixture>]
module ``Wikipedia - parsePage`` =

    let testPargePage source expected =
        let actual = Wikipedia.parsePage source
        printfn "actual : %A" actual
        actual |> should equal expected

    [<Test>]
    let ``グレゴリオ暦の日付をパーズできること`` () =
        let source = """<li>1801年5月27日（享和元年4月15日） 上総地震 - 久留里城が破損。</li>"""
        let expected = [
            {
                OccuredDate = {DateTime = DateTime(1801, 5, 27); IncludesTime = false}
                Name = Some(EarthquakeName {Name = "上総地震"; AlternativeName = None})
                SeismicIntensity = None
                Magnitude = None
                MomentMagnitude = None
                Detail = "久留里城が破損。"
            }
        ]
        testPargePage source expected

    [<Test>]
    let ``年と月日が複数行にまたがったグレゴリオ暦の日付をパーズできること`` () =
        let source = """<li>1611年
<ul>
<li>9月27日（慶長16年8月21日） 会津地震 - 死者3,700人。</li>
<li>12月2日（慶長16年10月28日） 慶長三陸地震 - 伊達領で大津波による死者約2,000〜5,000人。</li>
</ul>
</li>"""
        let expected = [
            {
                OccuredDate = {DateTime = DateTime(1611, 9, 27); IncludesTime = false}
                Name = Some(EarthquakeName {Name = "会津地震"; AlternativeName = None})
                SeismicIntensity = None
                Magnitude = None
                MomentMagnitude = None
                Detail = "死者3,700人。"
            }
            {
                OccuredDate = {DateTime = DateTime(1611, 12, 2); IncludesTime = false}
                Name = Some(EarthquakeName {Name = "慶長三陸地震"; AlternativeName = None})
                SeismicIntensity = None
                Magnitude = None
                MomentMagnitude = None
                Detail = "伊達領で大津波による死者約2,000〜5,000人。"
            }
        ]
        testPargePage source expected

    [<Test>]
    let ``時間を含むグレゴリオ暦の日付をパーズできること`` () =
        let source = """<li>2004年（平成16年）
<ul>
<li>10月23日17時56分頃 新潟県中越地震 - 死者68人。川口町の地震計で当時世界最高の2,516ガルを記録。</li>
</ul>
</li>"""
        let expected = [
            {
                OccuredDate = {DateTime = DateTime(2004, 10, 23, 17, 56, 0); IncludesTime = true}
                Name = Some(EarthquakeName {Name = "新潟県中越地震"; AlternativeName = None})
                SeismicIntensity = None
                Magnitude = None
                MomentMagnitude = None
                Detail = "死者68人。川口町の地震計で当時世界最高の2,516ガルを記録。"
            }
        ]
        testPargePage source expected

    [<Test>]
    let ``和暦付き年に続いて月日があるグレゴリオ暦の日付をパーズできること`` () =
        let source = """<li>1997年（平成9年）5月13日 鹿児島県北西部地震 - 川内市で。</li>"""
        let expected = [
            {
                OccuredDate = {DateTime = DateTime(1997, 5, 13); IncludesTime = false}
                Name = Some(EarthquakeName {Name = "鹿児島県北西部地震"; AlternativeName = None})
                SeismicIntensity = None
                Magnitude = None
                MomentMagnitude = None
                Detail = "川内市で。"
            }
        ]
        testPargePage source expected

    [<Test>]
    let ``和暦付きの年と月日が複数行にまたがったグレゴリオ暦の日付をパーズできること`` () =
        let source = """<li>1894年（明治27年）
<ul>
<li>3月22日 根室半島沖地震 - 死者1人。北海道・東北に津波。</li>
<li>6月20日 明治東京地震 - 死者31人。</li>
<li>10月22日 庄内地震 - 死者726人。</li>
</ul>
</li>"""
        let expected = [
            {
                OccuredDate = {DateTime = DateTime(1894, 3, 22); IncludesTime = false}
                Name = Some(EarthquakeName {Name = "根室半島沖地震"; AlternativeName = None})
                SeismicIntensity = None
                Magnitude = None
                MomentMagnitude = None
                Detail = "死者1人。北海道・東北に津波。"
            }
            {
                OccuredDate = {DateTime = DateTime(1894, 6, 20); IncludesTime = false}
                Name = Some(EarthquakeName {Name = "明治東京地震"; AlternativeName = None})
                SeismicIntensity = None
                Magnitude = None
                MomentMagnitude = None
                Detail = "死者31人。"
            }
            {
                OccuredDate = {DateTime = DateTime(1894, 10, 22); IncludesTime = false}
                Name = Some(EarthquakeName {Name = "庄内地震"; AlternativeName = None})
                SeismicIntensity = None
                Magnitude = None
                MomentMagnitude = None
                Detail = "死者726人。"
            }
        ]
        testPargePage source expected

    [<Test>]
    let ``同日を含む複数行にまたがったグレゴリオ暦の日付をパーズできること`` () =
        let source = """<li>2004年（平成16年）
<ul>
<li>10月23日17時56分頃 新潟県中越地震 - 死者68人。川口町の地震計で当時世界最高の2,516ガルを記録。
<ul>
<li>同日18時11分頃 新潟県中越地震 - 新潟県中越地震の余震。</li>
<li>同日18時34分頃 新潟県中越地震 - 新潟県中越地震の最大余震。</li>
</ul>
</li>
</ul>
</li>"""
        let expected = [
            {
                OccuredDate = {DateTime = DateTime(2004, 10, 23, 17, 56, 0); IncludesTime = true}
                Name = Some(EarthquakeName {Name = "新潟県中越地震"; AlternativeName = None})
                SeismicIntensity = None
                Magnitude = None
                MomentMagnitude = None
                Detail = "死者68人。川口町の地震計で当時世界最高の2,516ガルを記録。"
            }
            {
                OccuredDate = {DateTime = DateTime(2004, 10, 23, 18, 11, 0); IncludesTime = true}
                Name = Some(EarthquakeName {Name = "新潟県中越地震"; AlternativeName = None})
                SeismicIntensity = None
                Magnitude = None
                MomentMagnitude = None
                Detail = "新潟県中越地震の余震。"
            }
            {
                OccuredDate = {DateTime = DateTime(2004, 10, 23, 18, 34, 0); IncludesTime = true}
                Name = Some(EarthquakeName {Name = "新潟県中越地震"; AlternativeName = None})
                SeismicIntensity = None
                Magnitude = None
                MomentMagnitude = None
                Detail = "新潟県中越地震の最大余震。"
            }
        ]
        testPargePage source expected

    [<Test>]
    let ``ユリウス暦の日付をパーズできること`` () =
        let source = """<li>416年8月22日（8月23日）（允恭5年7月14日） 允恭地震 - 遠飛鳥宮（大和国/現・奈良県明日香村）で地震。『日本書紀』に「地震」の記述。記録に残る日本史上最初の地震</li>"""
        let expected = [
            {
                OccuredDate = {DateTime = DateTime(416, 8, 23); IncludesTime = false}
                Name = Some(EarthquakeName {Name = "允恭地震"; AlternativeName = None})
                SeismicIntensity = None
                Magnitude = None
                MomentMagnitude = None
                Detail = "遠飛鳥宮（大和国/現・奈良県明日香村）で地震。『日本書紀』に「地震」の記述。記録に残る日本史上最初の地震"
            }
        ]
        testPargePage source expected

    [<Test>]
    let ``年と月日が複数行にまたがったユリウス暦のデータをパーズできること`` () =
        let source = """<li>1454年
<ul>
<li>12月12日（12月21日）（享徳3年11月23日夜半） 享徳地震 - 会津で強震、奥州海岸に大津波で人が多く流される。15世紀頃と見られる津波堆積物との関連性が指摘されている。</li>
<li>12月29日（1455年1月7日）（享徳3年12月10日） 享徳地震 - 鎌倉で余震とみられる大地震（『鎌倉大日記』）。</li>
</ul>
</li>"""
        let expected = [
            {
                OccuredDate = {DateTime = DateTime(1454, 12, 21); IncludesTime = false}
                Name = Some(EarthquakeName {Name = "享徳地震"; AlternativeName = None})
                SeismicIntensity = None
                Magnitude = None
                MomentMagnitude = None
                Detail = "会津で強震、奥州海岸に大津波で人が多く流される。15世紀頃と見られる津波堆積物との関連性が指摘されている。"
            }
            {
                OccuredDate = {DateTime = DateTime(1455, 1, 7); IncludesTime = false}
                Name = Some(EarthquakeName {Name = "享徳地震"; AlternativeName = None})
                SeismicIntensity = None
                Magnitude = None
                MomentMagnitude = None
                Detail = "鎌倉で余震とみられる大地震（『鎌倉大日記』）。"
            }
        ]
        testPargePage source expected

    [<Test>]
    let ``日付と地震名の間にスペースがないデータをパーズできること`` () =
        let source = """<li>1454年
<ul>
<li>12月12日（12月21日）（享徳3年11月23日夜半）享徳地震 - 会津で強震、奥州海岸に大津波で人が多く流される。15世紀頃と見られる津波堆積物との関連性が指摘されている。</li>
</ul>
</li>"""
        let expected = [
            {
                OccuredDate = {DateTime = DateTime(1454, 12, 21); IncludesTime = false}
                Name = Some(EarthquakeName {Name = "享徳地震"; AlternativeName = None})
                SeismicIntensity = None
                Magnitude = None
                MomentMagnitude = None
                Detail = "会津で強震、奥州海岸に大津波で人が多く流される。15世紀頃と見られる津波堆積物との関連性が指摘されている。"
            }
        ]
        testPargePage source expected

    [<Test>]
    let ``地震名がないデータをパーズできること`` () =
        let source = """<li>1454年
<ul>
<li>12月12日（12月21日）（享徳3年11月23日夜半） 会津で強震、奥州海岸に大津波で人が多く流される。15世紀頃と見られる津波堆積物との関連性が指摘されている。</li>
<li>12月29日（1455年1月7日）（享徳3年12月10日） - 鎌倉で余震とみられる大地震（『鎌倉大日記』）。</li>
</ul>
</li>"""
        let expected = [
            {
                OccuredDate = {DateTime = DateTime(1454, 12, 21); IncludesTime = false}
                Name = None
                SeismicIntensity = None
                Magnitude = None
                MomentMagnitude = None
                Detail = "会津で強震、奥州海岸に大津波で人が多く流される。15世紀頃と見られる津波堆積物との関連性が指摘されている。"
            }
            {
                OccuredDate = {DateTime = DateTime(1455, 1, 7); IncludesTime = false}
                Name = None
                SeismicIntensity = None
                Magnitude = None
                MomentMagnitude = None
                Detail = "鎌倉で余震とみられる大地震（『鎌倉大日記』）。"
            }
        ]
        testPargePage source expected

    [<Test>]
    let ``地震名に別名があるデータをパーズできること`` () =
        let source = """<li>1854年
<ul>
<li>12月23日（嘉永7年11月4日） 安政東海地震（東海・東南海地震） - 死者2,000〜3,000人。房総半島から四国に津波、特に伊豆から熊野にかけて大きな被害。ロシア船ディアナ号（プチャーチン提督来航）沈没。</li>
</ul>
</li>"""
        let expected = [
            {
                OccuredDate = {DateTime = DateTime(1854, 12, 23); IncludesTime = false}
                Name = Some(EarthquakeName {Name = "安政東海地震"; AlternativeName = Some "東海・東南海地震"})
                SeismicIntensity = None
                Magnitude = None
                MomentMagnitude = None
                Detail = "死者2,000〜3,000人。房総半島から四国に津波、特に伊豆から熊野にかけて大きな被害。ロシア船ディアナ号（プチャーチン提督来航）沈没。"
            }
        ]
        testPargePage source expected

    [<Test>]
    let ``地震名が地域名のデータをパーズできること`` () =
        let source = """<li>1896年（明治29年）
<ul>
<li>6月16日 三陸沖で地震 - 明治三陸地震の最大余震。</li>
</ul>
</li>"""
        let expected = [
            {
                OccuredDate = {DateTime = DateTime(1896, 6, 16); IncludesTime = false}
                Name = Some(Region "三陸沖")
                SeismicIntensity = None
                Magnitude = None
                MomentMagnitude = None
                Detail = "明治三陸地震の最大余震。"
            }
        ]
        testPargePage source expected

    [<Test>]
    let ``リストの深さが3のデータをパーズできること`` () =
        let source = """<li>1923年（大正12年）
<ul>
<li>9月1日11時58分頃 関東地震（大正関東地震、関東大震災） - 死者・行方不明者10万5,385人（1925年の調査では14万2,800人）（日本災害史上最悪）。
<ul>
<li>同日12時3分頃 相模湾で地震 - 大正関東地震の余震。</li>
</ul>
</li>
</ul>
</li>"""
        let expected = [
            {
                OccuredDate = {DateTime = DateTime(1923, 9, 1, 11, 58, 0); IncludesTime = true}
                Name = Some(EarthquakeName {Name = "関東地震"; AlternativeName = Some "大正関東地震、関東大震災"})
                SeismicIntensity = None
                Magnitude = None
                MomentMagnitude = None
                Detail = "死者・行方不明者10万5,385人（1925年の調査では14万2,800人）（日本災害史上最悪）。"
            }
            {
                OccuredDate = {DateTime = DateTime(1923, 9, 1, 12, 3, 0); IncludesTime = true}
                Name = Some(Region "相模湾")
                SeismicIntensity = None
                Magnitude = None
                MomentMagnitude = None
                Detail = "大正関東地震の余震。"
            }
        ]
        testPargePage source expected

    [<Test>]
    let ``旧震度をパーズできること`` () =
        let source = """<li>1995年（平成7年）
<ul>
<li>1月7日 岩手県沖で地震 - 青森県八戸市、岩手県盛岡市・葛巻町で最大震度 1、三陸はるか沖地震の最大余震。</li>
<li>1月7日 岩手県沖で地震 - 青森県八戸市、岩手県盛岡市・葛巻町で最大震度5、三陸はるか沖地震の最大余震。</li>
<li>1月17日 兵庫県南部地震（阪神・淡路大震災、阪神大震災） - 兵庫県南部・淡路島北部で最大震度 7、死者・行方不明者6,437人。当初は最大震度 6だったが、実地検分により7に修正された。</li>
<li>12月4日 択捉島南東沖で地震 - 北海道と青森県で最大震度 6。根室 17cm、八戸 13cm、釧路 10cmの津波を観測。</li>
</ul>
</li>
</ul>"""
        let expected = [
            {
                OccuredDate = {DateTime = DateTime(1995, 1, 7); IncludesTime = false}
                Name = Some(Region "岩手県沖")
                SeismicIntensity = Some(SeismicIntensityVer1 {Intensity = 1})
                Magnitude = None
                MomentMagnitude = None
                Detail = "青森県八戸市、岩手県盛岡市・葛巻町で最大震度 1、三陸はるか沖地震の最大余震。"
            }
            {
                OccuredDate = {DateTime = DateTime(1995, 1, 7); IncludesTime = false}
                Name = Some(Region "岩手県沖")
                SeismicIntensity = Some(SeismicIntensityVer1 {Intensity = 5})
                Magnitude = None
                MomentMagnitude = None
                Detail = "青森県八戸市、岩手県盛岡市・葛巻町で最大震度5、三陸はるか沖地震の最大余震。"
            }
            {
                OccuredDate = {DateTime = DateTime(1995, 1, 17); IncludesTime = false}
                Name = Some(EarthquakeName {Name = "兵庫県南部地震"; AlternativeName = Some "阪神・淡路大震災、阪神大震災"})
                SeismicIntensity = Some(SeismicIntensityVer1 {Intensity = 7})
                Magnitude = None
                MomentMagnitude = None
                Detail = "兵庫県南部・淡路島北部で最大震度 7、死者・行方不明者6,437人。当初は最大震度 6だったが、実地検分により7に修正された。"
            }
            {
                OccuredDate = {DateTime = DateTime(1995, 12, 4); IncludesTime = false}
                Name = Some(Region "択捉島南東沖")
                SeismicIntensity = Some(SeismicIntensityVer1 {Intensity = 6})
                Magnitude = None
                MomentMagnitude = None
                Detail = "北海道と青森県で最大震度 6。根室 17cm、八戸 13cm、釧路 10cmの津波を観測。"
            }
        ]
        testPargePage source expected

    [<Test>]
    let ``新震度をパーズできること`` () =
        let source = """<li>1997年（平成9年）5月13日 鹿児島県北西部地震 - 川内市で最大震度 6弱。</li>
<li>1998年（平成10年）
<ul>
<li>4月20日 伊豆半島東方沖で地震 - 東京都伊豆大島と静岡県熱海市・伊東市で最大震度5弱。</li>
<li>5月4日 石垣島南方沖地震 - 先島諸島で最大震度 1。一時津波警報発表。</li>
<li>8月20日 小笠原諸島西方沖で地震 - 埼玉県、千葉県、東京都で最大震度 5強。</li>
<li>9月3日 岩手県内陸北部地震 - 岩手県雫石町で最大震度 6強。</li>
<li>9月3日 岩手県内陸北部地震 - 岩手県雫石町で最大震度 7。</li>
</ul>
</li>
</ul>"""
        let expected = [
            {
                OccuredDate = {DateTime = DateTime(1997, 5, 13); IncludesTime = false}
                Name = Some(EarthquakeName {Name = "鹿児島県北西部地震"; AlternativeName = None})
                SeismicIntensity = Some(SeismicIntensityVer2 {Intensity = 6; IsUpper = Some false})
                Magnitude = None
                MomentMagnitude = None
                Detail = "川内市で最大震度 6弱。"
            }
            {
                OccuredDate = {DateTime = DateTime(1998, 4, 20); IncludesTime = false}
                Name = Some(Region "伊豆半島東方沖")
                SeismicIntensity = Some(SeismicIntensityVer2 {Intensity = 5; IsUpper = Some false})
                Magnitude = None
                MomentMagnitude = None
                Detail = "東京都伊豆大島と静岡県熱海市・伊東市で最大震度5弱。"
            }
            {
                OccuredDate = {DateTime = DateTime(1998, 5, 4); IncludesTime = false}
                Name = Some(EarthquakeName {Name = "石垣島南方沖地震"; AlternativeName = None})
                SeismicIntensity = Some(SeismicIntensityVer2 {Intensity = 1; IsUpper = None})
                Magnitude = None
                MomentMagnitude = None
                Detail = "先島諸島で最大震度 1。一時津波警報発表。"
            }
            {
                OccuredDate = {DateTime = DateTime(1998, 8, 20); IncludesTime = false}
                Name = Some(Region "小笠原諸島西方沖")
                SeismicIntensity = Some(SeismicIntensityVer2 {Intensity = 5; IsUpper = Some true})
                Magnitude = None
                MomentMagnitude = None
                Detail = "埼玉県、千葉県、東京都で最大震度 5強。"
            }
            {
                OccuredDate = {DateTime = DateTime(1998, 9, 3); IncludesTime = false}
                Name = Some(EarthquakeName {Name = "岩手県内陸北部地震"; AlternativeName = None})
                SeismicIntensity = Some(SeismicIntensityVer2 {Intensity = 6; IsUpper = Some true})
                Magnitude = None
                MomentMagnitude = None
                Detail = "岩手県雫石町で最大震度 6強。"
            }
            {
                OccuredDate = {DateTime = DateTime(1998, 9, 3); IncludesTime = false}
                Name = Some(EarthquakeName {Name = "岩手県内陸北部地震"; AlternativeName = None})
                SeismicIntensity = Some(SeismicIntensityVer2 {Intensity = 7; IsUpper = None})
                Magnitude = None
                MomentMagnitude = None
                Detail = "岩手県雫石町で最大震度 7。"
            }
        ]
        testPargePage source expected


    [<Test>]
    let ``M表記のマグニチュードをパーズできること`` () =
        let source = """<li>1611年
<ul>
<li>9月27日（慶長16年8月21日） 会津地震 - M 6.9、死者3,700人。</li>
<li>12月2日（慶長16年10月28日） 慶長三陸地震 - M8.1。十勝・根室沖のM 9クラスとする説がある。一方、東北地方太平洋側で繰り返し発生していると推定されるM 9クラスの地震の候補ともされる。伊達領で大津波による死者約2,000〜5,000人。</li>
</ul>
</li>"""
        let expected = [
            {
                OccuredDate = {DateTime = DateTime(1611, 9, 27); IncludesTime = false}
                Name = Some(EarthquakeName {Name = "会津地震"; AlternativeName = None})
                SeismicIntensity = None
                Magnitude = Some 6.9
                MomentMagnitude = None
                Detail = "M 6.9、死者3,700人。"
            }
            {
                OccuredDate = {DateTime = DateTime(1611, 12, 2); IncludesTime = false}
                Name = Some(EarthquakeName {Name = "慶長三陸地震"; AlternativeName = None})
                SeismicIntensity = None
                Magnitude = Some 8.1
                MomentMagnitude = None
                Detail = "M8.1。十勝・根室沖のM 9クラスとする説がある。一方、東北地方太平洋側で繰り返し発生していると推定されるM 9クラスの地震の候補ともされる。伊達領で大津波による死者約2,000〜5,000人。"
            }
        ]
        testPargePage source expected

    [<Test>]
    let ``Mj表記のマグニチュードをパーズできること`` () =
        let source = """<li>1611年
<ul>
<li>9月27日（慶長16年8月21日） 会津地震 - Mj 6.9、死者3,700人。</li>
<li>12月2日（慶長16年10月28日） 慶長三陸地震 - Mj8.1。十勝・根室沖のM 9クラスとする説がある。一方、東北地方太平洋側で繰り返し発生していると推定されるMj 9クラスの地震の候補ともされる。伊達領で大津波による死者約2,000〜5,000人。</li>
</ul>
</li>"""
        let expected = [
            {
                OccuredDate = {DateTime = DateTime(1611, 9, 27); IncludesTime = false}
                Name = Some(EarthquakeName {Name = "会津地震"; AlternativeName = None})
                SeismicIntensity = None
                Magnitude = Some 6.9
                MomentMagnitude = None
                Detail = "Mj 6.9、死者3,700人。"
            }
            {
                OccuredDate = {DateTime = DateTime(1611, 12, 2); IncludesTime = false}
                Name = Some(EarthquakeName {Name = "慶長三陸地震"; AlternativeName = None})
                SeismicIntensity = None
                Magnitude = Some 8.1
                MomentMagnitude = None
                Detail = "Mj8.1。十勝・根室沖のM 9クラスとする説がある。一方、東北地方太平洋側で繰り返し発生していると推定されるMj 9クラスの地震の候補ともされる。伊達領で大津波による死者約2,000〜5,000人。"
            }
        ]
        testPargePage source expected

    [<Test>]
    let ``モーメントマグニチュードをパーズできること`` () =
        let source = """<li>1677年
<ul>
<li>11月4日（延宝5年10月9日） 延宝房総沖地震（延宝地震） - M 8.0前後（Mw 8.5）、死者500〜600人。福島県〜千葉県に津波（茨城県地方史上最大の津波被害）。</li>
<li>11月4日（延宝5年10月9日） 延宝房総沖地震（延宝地震） - M 8.0前後（Mw8.5）、死者500〜600人。福島県〜千葉県に津波（茨城県地方史上最大の津波被害）。</li>
</ul>
</li>"""
        let expected = [
            {
                OccuredDate = {DateTime = DateTime(1677, 11, 4); IncludesTime = false}
                Name = Some(EarthquakeName {Name = "延宝房総沖地震"; AlternativeName = Some "延宝地震"})
                SeismicIntensity = None
                Magnitude = Some 8.0
                MomentMagnitude = Some 8.5
                Detail = "M 8.0前後（Mw 8.5）、死者500〜600人。福島県〜千葉県に津波（茨城県地方史上最大の津波被害）。"
            }
            {
                OccuredDate = {DateTime = DateTime(1677, 11, 4); IncludesTime = false}
                Name = Some(EarthquakeName {Name = "延宝房総沖地震"; AlternativeName = Some "延宝地震"})
                SeismicIntensity = None
                Magnitude = Some 8.0
                MomentMagnitude = Some 8.5
                Detail = "M 8.0前後（Mw8.5）、死者500〜600人。福島県〜千葉県に津波（茨城県地方史上最大の津波被害）。"
            }
        ]
        testPargePage source expected

    [<Test>]
    let ``見出しを無視してパーズを続行できること`` () =
        let source = """<ul>
<li>2000年（平成12年）
<ul>
<li>1月28日 根室半島南東沖で地震 - Mj 7.0（Mw 6.8）、深さ59km、北海道釧路市・厚岸町・中標津町・別海町・根室市で最大震度 4。</li>
</ul>
</li>
</ul>
<h2>21世紀[編集]</h2>
<p>注: 2003年9月17日に気象庁マグニチュードの算出方法が改訂された。これにより、マグニチュードが改訂された地震については改訂前の値を括弧書きで併記している（例: 2001年3月24日 芸予地震 - Mj 6.7（旧Mj 6.4））。</p>
<h3>2000年代[編集]</h3>
<ul>
<li>2001年（平成13年）
<ul>
<li>3月24日 芸予地震 - Mj 6.7（旧Mj 6.4）（Mw 6.8）、広島県河内町・大崎上島町、熊野町で最大震度 6弱、死者2人。フィリピン海プレート内部の地震。</li>
</ul>
</li>"""
        let expected = [
            {
                OccuredDate = {DateTime = DateTime(2000, 1, 28); IncludesTime = false}
                Name = Some(Region "根室半島南東沖")
                SeismicIntensity = Some(SeismicIntensityVer2 {Intensity = 4; IsUpper = None})
                Magnitude = Some 7.0
                MomentMagnitude = Some 6.8
                Detail = "Mj 7.0（Mw 6.8）、深さ59km、北海道釧路市・厚岸町・中標津町・別海町・根室市で最大震度 4。"
            }
            {
                OccuredDate = {DateTime = DateTime(2001, 3, 24); IncludesTime = false}
                Name = Some(EarthquakeName {Name = "芸予地震"; AlternativeName = None})
                SeismicIntensity = Some(SeismicIntensityVer2 {Intensity = 6; IsUpper = Some false})
                Magnitude = Some 6.7
                MomentMagnitude = Some 6.8
                Detail = "Mj 6.7（旧Mj 6.4）（Mw 6.8）、広島県河内町・大崎上島町、熊野町で最大震度 6弱、死者2人。フィリピン海プレート内部の地震。"
            }
        ]
        testPargePage source expected
