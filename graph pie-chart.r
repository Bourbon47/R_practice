# pie chart

pie(c(152,5,3,13,127), col=c("red3","blue","green3","magenta","yellow"),
    labels=c("������ 152","","�� 3","���� 13","���� 127"), radius = 0.9, 
    init.angle=90,clockwise=T, border=NA, main="19�� ��ȸ�ǿ� ����")
text(0,-1,"���� 5")

par(new=T)
pie(c(152,127,13,5,3), radius=0.8, col = "white",label=NA, border=NA)
text(0,0,"�� 300��")

# end
