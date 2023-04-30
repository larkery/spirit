duration = hours | minutes | seconds
hours = integer <#" +" "hour" "s"?> (<#" +"> minutes)?
minutes = integer <#" +" "minute" "s"?> (<#" +"> seconds)?
seconds = integer <#" +" "second" "s"?>

time = ((hour_of_day  (<#" +"> minute_of_hour)?) | (minute_prefix <#" +"> hour_of_day)) (<#" *"> am_or_pm)?;
hour_of_day = integer;
minute_of_hour = integer;
minute_prefix = ("quarter" | "half" | integer) <#" +"> ("to" | "past");
<am_or_pm> = "am" | "pm";

integer = #"\d+";
