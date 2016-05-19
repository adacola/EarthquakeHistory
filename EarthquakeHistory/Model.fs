module Adacola.EarthquakeHistory.Model

open System

/// 発生日時
type OccuredDate = {
    /// 発生日、または発生日時
    DateTime : DateTime
    /// 発生した時刻もデータに含まれているかどうか。trueの場合はDateTimeのTime部分にも意味がある(精度は分まで)。falseの場合はDateTimeのDate部分のみ意味がある(Time部分はデフォルトの00:00:00)。
    IncludesTime : bool
}

/// 1996年10月以前の震度
type SeismicIntensityVer1 = {
    /// 震度の数字部分
    Intensity : int
}

/// 1996年10月以降の震度
type SeismicIntensityVer2 = {
    /// 震度の数字部分
    Intensity : int
    /// 震度5～6にて、弱の場合はSome(false)、強の場合はSome(true)。震度5～6以外の場合はNone
    IsUpper : bool option
}

/// 震度
type SeismicIntensity = SeismicIntensityVer1 of SeismicIntensityVer1 | SeismicIntensityVer2 of SeismicIntensityVer2
with
    static member Create(dateTime, intensity, isUpper) =
        let ``震度改定の日付`` = DateTime(1996, 10, 1)
        if dateTime < ``震度改定の日付`` then
            if Option.isSome isUpper then failwith "震度の改定以前に強または弱の指標は出てきません"
            else SeismicIntensityVer1 {Intensity = intensity}
        else SeismicIntensityVer2 {Intensity = intensity; IsUpper = isUpper}

/// 地震名
type EarthquakeName = {
    Name : string
    AlternativeName : string option
}

type Name = EarthquakeName of EarthquakeName | Region of string

/// 地震のデータ
type Earthquake = {
    /// 発生日時
    OccuredDate : OccuredDate
    /// 地震名
    Name : Name option
    /// 震度
    SeismicIntensity : SeismicIntensity option
    /// マグニチュード。通常は気象庁マグニチュード
    Magnitude : float option
    /// モーメントマグニチュード
    MomentMagnitude : float option
    /// 詳細
    Detail : string
}
