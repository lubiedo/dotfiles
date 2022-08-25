rule NAME : TAGS {
  meta:
    author      = "@_lubiedo"
    date        = "dd-mm-yyyy"
    description = 
    hash0       = ""
  strings:
    $s00 = "" nocase
  condition:
    filesize < 100KB and all of ($s*)
}
