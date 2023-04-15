time = hours | minutes | seconds
hours = integer <#" +" "hour" "s"?> (<#" +"> minutes)?
minutes = integer <#" +" "minute" "s"?> (<#" +"> seconds)?
seconds = integer <#" +" "second" "s"?>
integer = #"\d+";
