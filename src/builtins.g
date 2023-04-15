time = hours | minutes | seconds
hours = integer <#" +"> <#"hours?"> (<#" +"> minutes)?
minutes = integer <#" +"> <#"minutes?"> (<#" +"> seconds)?
seconds = integer <#" +"> <#"seconds?">
integer = #"\d+";
