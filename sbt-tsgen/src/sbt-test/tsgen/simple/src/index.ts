import * as t from "io-ts"
import * as P from "../src_generated/polaris-1.0"
import * as F from "../src_generated/fes-2.0"

let x: P.ScanInfoT = {
  mimeType: "application/octet-stream",
  location: "unknown"
}

console.log(t.validate(x, P.ScanInfoTType))

let y: F.DateIntervalT = {
  from: new Date()
}

console.log(t.validate(y, F.DateIntervalTType))
