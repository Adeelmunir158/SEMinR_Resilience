

install.packages("goftest")
install.packages("seminr")
require(readxl)
require(seminr)
attach(Resilience2)
View(Resilience2)
#===========================================

measurements = constructs(
  composite("Technical",multi_items("TC",1:3)),
  composite("Management",multi_items("MC",1:3)),
  composite("Human Resource",multi_items("HC",1:3)),
  composite("Data driven",multi_items("DD",1:3)),
 higher_composite("SCAC",c("Technical","Management","Human Resource","Data driven"), method = "two stage"),  
  
 composite("Absorptive",multi_items("Absorp",1:5)),
 composite("Adaptive",multi_items("Adapt",1:5)),
 composite("Restorative",multi_items("Restore",1:3)),
 composite("Social Capital",multi_items("Social",1:4)),
 
  higher_composite("Resilience",c("Absorptive","Adaptive","Restorative","Social Capital"), method = "two stage"),  
  
 composite("Risk Management",multi_items("Risk",1:5)),
 composite("Exploitative",multi_items("Exploit",1:4)),
 composite("Explorative",multi_items("Explore",1:4)),
higher_composite("Ambidexterity",c("Exploitative","Explorative"), method = "two stage"))

#------------------------------------------
# Quickly create multiple paths "from" and "to" sets of constructs

plot(measurements)

structure_pls = relationships(
  paths(from = "Ambidexterity", to = "SCAC"),
  paths(from = "SCAC", to = "Resilience"),
  paths(from = "Risk Management", to = c("Resilience", "SCAC")),
  paths(from = "Ambidexterity", to = "Resilience"))

plot(structure_pls)



#===========================================

pls_model = estimate_pls(Resilience2,
                         measurements,
                         structure_pls)


# PLSPredict

p=predict_pls(pls_model, technique = predict_DA, noFolds = 10)

pp=summary(p)
pp


plot_scores(pls_model)
plot(pls_model, cex=3)
s=summary(pls_model)

s$loadings
s$descriptives
#Validity
s$validity
s$total_effects
s$it_criteria
s$fSquare
s$reliability
s$validity$fl_criteria
s$weights
s$vif_antecedents$Ambidexterity
s$descriptives$statistics$items
s$paths
#Reliability
sink("mock_test.doc")
s$reliability
sink()
#Loadings
s$loadings
s$iterations
s$loadings
#Structural Model
s$paths
s$total_effects
s$total_indirect_effects
s$vif_antecedents
s$fSquare
s$it_criteria
s$composite_scores
s$validity$htmt
# Bootstrap
p5=bootstrap_model(pls_model,nboot = 5000)
s2=summary(p5)
s2
s2$bootstrapped_HTMT
#===========================================
