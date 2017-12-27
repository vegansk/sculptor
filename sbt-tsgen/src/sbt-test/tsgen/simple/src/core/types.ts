import * as t from "io-ts"

export const date: t.Type<Date> = {
  _A: t._A,
  name: "Date",
  validate: (v, c) => {
    if (!(v instanceof Date) && typeof v !== "string") {
      return t.failure<Date>(v, c)
    }

    const d = v instanceof Date ? v : new Date(v)
    return isNaN(d.getTime()) ? t.failure<Date>(v, c) : t.success(d)
  }
}
