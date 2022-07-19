# pie chart

pie(c(152,5,3,13,127), col=c("red3","blue","green3","magenta","yellow"),
    labels=c("새누리 152","","무 3","진보 13","민주 127"), radius = 0.9, 
    init.angle=90,clockwise=T, border=NA, main="19대 국회의원 선거")
text(0,-1,"선진 5")

par(new=T)
pie(c(152,127,13,5,3), radius=0.8, col = "white",label=NA, border=NA)
text(0,0,"총 300석")

# end
