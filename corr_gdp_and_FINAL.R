# Correlation between GDP Growth Rate & Voter Turnout
gdp <- c(25.66,
         15.77,
         16.36,
         05.89,
         24.44
)
turnout <- c(0.4580,
            0.5620,
            0.6250,
            0.5900,
            0.7600
)
cor(gdp, turnout)

# The resulting Cor-Coefficient is .03029178
# This is a positive correlation but the strength is a rather weak uphill
# So, in the case of GA the correlation between GDP and Voter turnout is positive and is a weak uphill

head(cbind(gdp, turnout)) 
plot(gdp, turnout, xlab="Georiga's GDP Growth Rate (in %)", ylab="Georgia's Turnout")
abline(lm(turnout ~ gdp))