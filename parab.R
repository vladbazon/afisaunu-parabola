draw_segment <- function(z2, z1, ...) {  # uneşte punctele de afixe z1, z2
    segments(Re(z1), Im(z1), Re(z2), Im(z2), ...) }

###  Verifică "bănuiala" că −∆/4a este tocmai focarul parabolei:
# piciorul perpendicularei din F(−∆/4a) pe o tangentă oarecare a parabolei Z(t)=at^2+bt+c (coeficienţi complecşi) este situat pe tangenta în vârful acesteia

a <- -3-2.2i
b <- 5+1.2i
c <- 1+2i
dt <- b^2 - 4*a*c
f <- -dt/(4*a)  # focarul
v <- a/16 * (b/a - Conj(b/a))^2 + f  # vârful parabolei

Z <- function(t) {  # z(t) pentru o valoare t, sau pentru o secvenţă de valori t
    a*t^2 + b*t +c
}
Zp <- function(t) {  # derivata lui Z
    2*a*t + b
}
axa <- function(t) {  # axa parabolei
    f + a*t
}
tgt <- function(t, tau) {  # tangenta în punctul Z(t)
    Z(t) + Zp(t)*tau
}
prf <- function(t) {  #proiecţia focarului pe tangenta în Z(t)
    0.5*(Z(t)+f) + 0.5*Zp(t)/Conj(Zp(t))*(Conj(f-Z(t)))
}
tgv <- function(t) {  # tangenta în vârf
    v + 1i*a*t
}

t_R <- seq(-0.5, 1.5, by=0.01)  # secvenţă t pentru "întreaga" parabolă
t_01 <- seq(0, 1, by=0.01)  # secvenţă t pentru un arc de parabolă

plot(Z(t_R), asp=1, type="n", bty="n", cex.axis=0.8, 
     xlim=c(0, 4), ylim=c(0, 4)) # iniţializează fereastra grafică
grid()

points(Z(t_R), type="l", lwd=1)  # trasează parabola
points(c(v, Z(0), Z(1), Z(0.35)), pch=19, cex=0.4)
points(f, col="gray20", cex=0.5)
text(c(f+0.01+0.03i, v-(0.06+1i*0.01)), labels=c("F", "V"), pos=c(1, 4), cex=0.9)
points(axa(t_R), type="l", lwd=0.5) #, col="blue")
points(tgv(t_R), type="l", lwd=0.5)
draw_segment(f, prf(0), lwd=0.3, lty="dashed")
draw_segment(f, prf(1), lwd=0.3, lty="dashed")
draw_segment(f, prf(0.35), lwd=0.3, lty="dashed")

for(h in c(0.35, 0, 1)) {
    points(tgt(h, t_R), type="l", lwd=0.5, col="gray35")
}
for(h in c(0.35, 0, 1)) {
    points(prf(h), col="gray30", cex=0.4)
}
#for(h in seq(-0.4, 1.4, by=0.1)) {  # proiecţiile focarului pe mai multe tangente
#    points(prf(h), col="firebrick2", pch=19, cex=0.2)
#}

points(b/a-Conj(b/a), cex=0.3)  # intră în exprimarea focarului, vârfului, etc.
text(b/a-Conj(b/a)-0.09, labels=expression(delta), pos=4, cex=1)

