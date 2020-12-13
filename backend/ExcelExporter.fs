module ExcelExporter

open System.IO
open NPOI.HSSF.UserModel

module D = Core.Domain
module F = Core.Domain.Format

let private heads =
    [ "№ поля"
      "Площать, гектары"
      "Крутизна склона, градусы"
      "Уровень плодородия почвы"
      "Наличие многолетних сорняков в 2020г"
      "Статус собственности\nна 2020 г"
      "Расстоние до асфальта, км"
      "Массив полей" ]

let export path fields =
    let doc = HSSFWorkbook ()
    let sheet = doc.CreateSheet "Результаты расчетов"
    let row = sheet.CreateRow 0

    heads
    |> List.iteri (fun i x ->
        let cell = row.CreateCell i
        cell.SetCellValue x )

    fields
    |> List.iteri (fun i (f : D.Field) ->
        let row = sheet.CreateRow (i + 1)

        let col = ref 0
        let writeText x =
            (row.CreateCell !col).SetCellValue (string x)
            incr col

        writeText f.id
        writeText f.area
        writeText f.angle
        writeText <| F.fertilityToString f
        writeText <| F.weedsToString f
        writeText <| F.propertyToString f
        writeText f.distance
        writeText f.cluster)

    let file = new FileStream(path, FileMode.Create)
    doc.Write file
