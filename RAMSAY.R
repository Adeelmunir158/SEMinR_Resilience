require(lmtest)
attach(Structural_model)
View(Structural_model)


resettest(Resilience~SCAC+`Risk Management`+Ambidexterity, power = 2:3, type = "regressor")


resettest(SCAC~`Risk Management`+Ambidexterity, power = 2:3, type = "regressor")






