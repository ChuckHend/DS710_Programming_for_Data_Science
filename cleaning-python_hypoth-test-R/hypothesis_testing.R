

# survey 100 people and find 61 who support raising minimum wage
prop.test(61, 100, p=0.5, alternative = 'greater')

# You want to know whether the unemployment rate is the same
# in Minnesota and Wisconsin. Using good random-sampling techniques, 
# you survey 500 people in Minnesota and find 19 who are unemployed. 
# You also survey 400 people in Wisconsin and find 17 who are unemployed.
prop.test( c(19, 17), c(500, 400), alternative = "two.sided" )


# The CDC recommends that adults get at least 150 minutes of moderate-intensity 
# exercise per week. You suspect that people in your city typically do not meet 
# this recommended amount, so you gather data to test your suspicions. You ask 70 
# people in your city to wear an activity monitor for a week, and record the number
# of minutes of exercise each person does in the vector minutes.
t.test(minutes, mu = 150, alternative = "less") 

.9*2.84155+6.44488
