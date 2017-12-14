import * as t from "io-ts"
import * as P from "../src_generated/polaris-1.0"

let x: P.ScanInfoT = {
  mimeType: "application/octet-stream",
  location: "unknown"
}

console.log(t.validate(x, P.ScanInfoTType))
