import * as t from "io-ts"
export class DateType extends t.Type<Date> {
  readonly tag: "DateType" = "DateType"
  constructor() {
    super(
      "Date",
      (d): d is Date => typeof d === "string" || d instanceof Date,
      (v, c) => {
        if (!(v instanceof Date) && typeof v !== "string") {
          return t.failure<Date>(v, c)
        }

        const d = v instanceof Date ? v : new Date(v)
        return isNaN(d.getTime()) ? t.failure<Date>(v, c) : t.success(d)
      },
      t.identity
    )
  }
}

export const date: DateType = new DateType()
