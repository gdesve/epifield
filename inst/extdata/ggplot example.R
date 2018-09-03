cat(red("test"))
cat(green("vert"))

theme1 <- theme(plot.title=element_text(size=30, face="bold"),  
                   axis.text.x=element_text(size=15), 
                   axis.text.y=element_text(size=15),
                   axis.title.x=element_text(size=25),
                   axis.title.y=element_text(size=25))

ggplot(cars, aes(x=speed,y=dist), color="blue")+geom_point()+geom_smooth()+
   labs(title="Scatterplot", x="Speed", y="Distance") + theme1
   

ggplot(cars, aes(x=speed)) + geom_bar() + labs(title="Frequency bar chart") +
theme1 

#  coord_flip()
ggplot(cars, aes(x=speed)) + geom_bar() + labs(title="Frequency bar chart") +
theme1 + coord_flip()

# ylim(c(0, 10000))
plot1 <- ggplot(cars, aes(x=speed)) + geom_bar() + labs(title="Frequency bar chart") +
theme1 + xlim(c(0,30))
print(plot1)

# Grilles
theme2 <- theme(panel.background = element_rect(fill = 'steelblue'),
  panel.grid.major = element_line(colour = "firebrick", size=3),
  panel.grid.minor = element_line(colour = "blue", size=1))

ggplot(cars, aes(x=speed)) + geom_bar() + theme2

ggsave("myggplot.png")  # saves the last plot.
ggsave("myggplot.png", plot=plot1)  # save a stored ggplot

mod <- lm(mpg ~ wt, data = mtcars)
qplot(resid(mod), fitted(mod))
 
qplot(y=speed, data=cars)




