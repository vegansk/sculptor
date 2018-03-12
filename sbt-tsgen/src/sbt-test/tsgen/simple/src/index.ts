import * as t from "io-ts"
import * as P from "../src_generated/polaris-1.0"
import * as F from "../src_generated/fes-2.0"

let x: P.ScanInfoT = {
  mimeType: "application/octet-stream",
  location: "unknown"
}

console.log(t.decode(x, P.ScanInfoTType))

let y: F.DateIntervalT = {
  from: new Date()
}

console.log(t.decode(y, F.DateIntervalTType))
