module TimeUtil where


import Time exposing (Time)
import Date exposing (Date)
import String exposing (padLeft)


type DateFormat
  = Dash_DMY
  | Slash_YMD

timeToDateString : DateFormat -> Time -> String
timeToDateString dateFormat time =
  let date = Date.fromTime time
  in let dayAsInt = Date.day date
         monthAsInt = monthToInt date
         yearAsInt = Date.year date
     in case dateFormat of
       Dash_DMY ->
         combineWithDash dayAsInt monthAsInt yearAsInt
       Slash_YMD ->
         combineWithSlash dayAsInt monthAsInt yearAsInt

combineWithDash : Int -> Int -> Int -> String
combineWithDash day month year =
  let dayString = toString day
      monthString
        = toString month
          |> monthAsTwoIntegers
      yearString = toString year
  in dayString ++ "-" ++ monthString ++ "-" ++ yearString

combineWithSlash : Int -> Int -> Int -> String
combineWithSlash day month year =
  let dayString = toString day
      monthString
        = toString month
          |> monthAsTwoIntegers
      yearString = toString year
  in dayString ++ "/" ++ monthString ++ "/" ++ yearString

monthToInt : Date -> Int
monthToInt date =
  case Date.month date of
    Date.Jan -> 1
    Date.Feb -> 2
    Date.Mar -> 3
    Date.Apr -> 4
    Date.May -> 5
    Date.Jun -> 6
    Date.Jul -> 7
    Date.Aug -> 8
    Date.Sep -> 9
    Date.Oct -> 10
    Date.Nov -> 11
    Date.Dec -> 12

monthAsTwoIntegers : String -> String
monthAsTwoIntegers month =
  padLeft 2 '0' month

stringToTime : String -> Time
stringToTime dateStr =
  let result = Date.fromString dateStr
  in case result of
    Ok dateValue ->
      Date.toTime dateValue
    Err errorString ->
      0

--clockSignal : Signal Time
--clockSignal = Time.every Time.second
