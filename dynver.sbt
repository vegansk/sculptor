// Derive version number from Git history

// Use -SNAPSHOT suffix
ThisBuild / dynverSonatypeSnapshots := true
  // Check that we're running from a Git repository
Global / onLoad := (Global / onLoad).value.andThen { s =>
  "dynverAssertTagVersion" :: s
}
