module MeetupProgram

open System


type Schedule = 
  | First
  | Second 
  | Third
  | Fourth 
  | Teenth
  | Last


let TEENTH_DAYS = seq {13..19}

let toDateTime year month day =
  new DateTime(year, month, day)


let populateDates year month days =
  days
  |> Seq.map (toDateTime year month)


let filterDate dayOfWeek (date: DateTime)= 
  date.DayOfWeek = dayOfWeek


let getDates year month dayOfWeek days  = 
  populateDates year month days 
  |> Seq.filter (filterDate dayOfWeek)


let meetupDay dayOfWeek schedule year month = 
  let days = seq { 1..DateTime.DaysInMonth(year, month) }
  let find n = getDates year month dayOfWeek >> Seq.item (n - 1)
  let findLast = getDates year month dayOfWeek >> Seq.last

  match schedule with 
  | First -> find 1 days
  | Second -> find 2 days
  | Third -> find 3 days
  | Fourth -> find 4 days
  | Teenth -> find 1 TEENTH_DAYS
  | Last -> findLast days
