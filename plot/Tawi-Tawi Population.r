library(ggplot2)
library(colorRamps)

TawiTawiPop <- c(17000, 45000, 46000, 59000, 79000, 110000, 143000, 195000, 228204,
                 250718, 322317, 450346, 366550)
YearNames <- c("1903", "1918", "1939", "1948", "1960", "1970", "1975", "1980", "1990",
               "1995", "2000", "2007", "2010")

qplot(YearNames, TawiTawiPop, 
      xlab = expression(bold("Censal Year")), 
      ylab = expression(bold("Population")), 
      geom = "bar",
      stat = "identity", colour = I("red"), 
      fill = matlab.like2(13), ylim = c(0, 520000)) + theme_bw() + 
        opts(
          title = expression(bold("Tawi-Tawi Population from 1903 to 2010")),
          plot.title = theme_text(size = 18, colour = "darkblue"),
          legend.position = "none",
          panel.border = theme_rect(linetype = "dashed", colour = "red"),
          plot.title = theme_text(size = 18, colour = "darkblue")) +
            geom_text(aes(label = TawiTawiPop, angle = 90, hjust = -0.1), size = 4)