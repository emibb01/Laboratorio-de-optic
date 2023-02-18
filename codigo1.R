df<-read.csv("acrilico_aire.csv")
df1<-df%>%select(Incidencia,Reflejado)
df2<-df%>%select(Incidencia,Refractado)
lm_fit1 <- lm(Reflejado ~ Incidencia, data = df1)
lm_fit2 <- lm(Refractado ~ Incidencia, data = df2)
slope1 <- lm_fit1$coefficients[2]
intercept1 <- lm_fit1$coefficients[1]
slope2 <- lm_fit2$coefficients[2]
intercept2 <- lm_fit2$coefficients[1]
plot1<-ggplot(df, aes(x=Incidencia,y=Reflejado))+geom_point()+geom_abline(intercept = intercept1, slope = slope1, color = "red")+
  geom_errorbar(aes(ymin=Reflejado-0.5, ymax=Reflejado+0.5),width=.2,position=position_dodge(0.05))
plot2<-ggplot(df, aes(x=Incidencia,y=Refractado))+geom_point()+geom_abline(intercept = intercept2, slope = slope2, color = "blue")+
  geom_errorbar(aes(ymin=Refractado-0.5, ymax=Refractado+0.5),width=.2,position=position_dodge(0.05))
grid.arrange(plot1, plot2, ncol = 2)
Sen1<-sin((df$Incidencia/180)*pi)
Sen2<-sin((df$Refractado/180)*pi)
error<-sqrt(((cos((df$Incidencia/180)*pi)/Sen2)*((0.5/180)*pi))^{2}+(((Sen1*cos((df$Refractado/180)*pi))/Sen2^{2})*((0.5/180)*pi)))^{2}
df3<-data.frame(Sen1=Sen1,
                Sen2=Sen2)
n1<-Sen1/Sen2
df3$n1<-n1
df3$Error<-error
ggplot(df3, aes(x=Sen2,y=Sen1))+geom_point()+geom_abline(intercept = intercept1, slope = slope1, color = "red")+geom_line()
write.csv(df3,"resultados_2.csv")
colMeans(df3,na.rm = "True")
df4<-df3[-c(1:13), ]
colMeans(df4,na.rm = "True")
