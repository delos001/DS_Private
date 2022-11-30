library(plotly)

z <- c(
  c(8.83,8.89,8.81,8.87,8.9,8.87),
  c(8.89,8.94,8.85,8.94,8.96,8.92),
  c(8.84,8.9,8.82,8.92,8.93,8.91),
  c(8.79,8.85,8.79,8.9,8.94,8.92),
  c(8.79,8.88,8.81,8.9,8.95,8.92),
  c(8.8,8.82,8.78,8.91,8.94,8.92),
  c(8.75,8.78,8.77,8.91,8.95,8.92),
  c(8.8,8.8,8.77,8.91,8.95,8.94),
  c(8.74,8.81,8.76,8.93,8.98,8.99),
  c(8.89,8.99,8.92,9.1,9.13,9.11),
  c(8.97,8.97,8.91,9.09,9.11,9.11),
  c(9.04,9.08,9.05,9.25,9.28,9.27),
  c(9,9.01,9,9.2,9.23,9.2),
  c(8.99,8.99,8.98,9.18,9.2,9.19),
  c(8.93,8.97,8.97,9.18,9.2,9.18)
)
dim(z) <- c(15,6)
z2 <- z + 1
z3 <- z - 1

fig <- plot_ly(showscale = FALSE)
fig <- fig %>% add_surface(z = ~z)
fig <- fig %>% add_surface(z = ~z2, opacity = 0.98)
fig <- fig %>% add_surface(z = ~z3, opacity = 0.98)

fig

set.seed(123)
a = c(runif(5, 4, 7),
      runif(5, 4, 7),
      runif(5, 4, 7),
      runif(5, 4, 7),
      runif(5, 4, 7),
      runif(5, 4, 7),
      runif(5, 4, 7),
      runif(5, 4, 7))

dim(a) <- c(8,5)

a2 = a + 3
a3 = a - 3

afig <- plot_ly(showscale = FALSE)
afig <- afig %>% add_surface(z = ~a)
afig <- afig %>% add_surface(z = ~a2, opacity = 0.98)
afig <- afig %>% add_surface(z = ~a3, opacity = 0.98)
afig

###########################################################################

set.seed(123)
a = c(c(runif(3, 9, 10), runif(3, 5, 6)), #runif(3, 7, 8)),
      c(runif(3, 9, 10), runif(3, 5, 6)), #runif(3, 7, 8)),
      c(runif(3, 9, 10), runif(3, 5, 6)), #runif(3, 7, 8)),
      c(runif(3, 9, 10), runif(3, 5, 6)), #runif(3, 7, 8)),
      c(runif(3, 9, 10), runif(3, 5, 6)), #runif(3, 7, 8)),
      c(runif(3, 9, 10), runif(3, 5, 6)), #runif(3, 7, 8)),
      c(runif(3, 9, 10), runif(3, 5, 6)), #runif(3, 7, 8)),
      c(runif(3, 9, 10), runif(3, 5, 6)) #runif(3, 7, 8))
)

dim(a) <- c(8,6)

a2 = a + 3
a3 = a - 3

afig <- plot_ly(showscale = FALSE)
afig <- afig %>% add_surface(z = ~a)
afig <- afig %>% add_surface(z = ~a2, opacity = 0.98)
afig <- afig %>% add_surface(z = ~a3, opacity = 0.98)
afig


###########################################################################

set.seed(123)
x = c(c(runif(20, 9, 10), runif(20, 5, 6), runif(20, 7, 8))
      )

y = c(c(runif(20, 9, 10), runif(20, 5, 6), runif(20, 7, 8))
)

z = c(c(runif(20, 9, 10), runif(20, 5, 6), runif(20, 7, 8))
)
pdata = data.frame(x=x, y=y, z=z)


plot_ly() %>% 
  add_trace(data = pdata,  x=pdata$x, y=pdata$y, z=pdata$z, type="mesh3d" ) 


###########################################################################

set.seed(123)
x = runif(200, 9, 10)

y = runif(200, 12, 14)

z = runif(200, 16, 18)

pdata = as.matrix(c(x, y, z))
dim(pdata) = c(200,3)


plot_ly(z = ~volcano) %>% 
  add_surface()

plot_ly(z = ~pdata) %>%
  add_surface()


###########################################################################

z = (1/volcano)

plot_ly(z = ~z, scene = 'scene2') %>% 
  add_surface(showscale = FALSE) %>%
  layout(title = 'Gradient Descent',
         xaxis = list(title = 'x'),
         yaxis = list(title = 'y'))



###########################################################################
cone <- function(x, y){
  sqrt(x^2+y^2)
  }
x = y = seq(-1, 1, length = 20)
z = outer(x, y, cone)
persp(x, y, z)


###########################################################################
cone <- function(x, y){
  sqrt(x^(1/2)+y^(1/2))
}
x = y = seq(-1, 1, length = 20)
z = outer(x, y, cone)
plot_ly(z = ~z, scene = 'scene2') %>%
  add_surface(showscale = FALSE)


###########################################################################
cone <- function(x, y){
  sqrt(abs((x^(2)-y^(2))))
}
x = y = seq(-1, 1, length = 20)
z = outer(x, y, cone)
plot_ly(z = ~z, scene = 'scene2') %>%
  add_surface(showscale = FALSE)


###########################################################################
cone <- function(x, y){
  sqrt((x^(3)*y^(3)))
}
x = y = seq(-1, 1, length = 20)
z = outer(x, y, cone)
plot_ly(z = ~z, scene = 'scene2') %>%
  add_surface(showscale = FALSE)
