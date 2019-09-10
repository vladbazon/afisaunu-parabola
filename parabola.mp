prologues:=3;  % asigură înglobarea fonturilor în PDF
verbatimtex  % pentru LaTex, când va fi invocat pentru a produce etichetele
    \documentclass[10pt]{article}
    \usepackage{lmodern}
    \usepackage{amssymb,amsmath}
    \begin{document}
etex

% arcul parabolei y=x^2, de capete date prin abscise (cubică Bézier)
vardef parstd(expr s, r) =
    pair P, Q, H, F, G;
    P = (s, s**2); Q = (r, r**2);  % puncte ale parabolei y=x^2
    H = ((s+r)/2, s*r);  % intersecţia tangentelor în capetele corzii
    F = (2*H + P)/3;  % punctele de control ale cubicei Bézier asociate
    G = (2*H + Q)/3;
    P .. controls F and G .. Q  % arcul parabolei standard, de capete P, Q
enddef;

vardef affinity(expr aPorA, bPorA, cP) =  % 3 puncte, sau 2 unghiuri şi un punct
    save a, b, c, T; 
    pair a, b, c; transform T;
    c = cP;
    if pair(aPorA) and pair(bPorA):  % dacă 'a' şi 'b' sunt date ca puncte
        a = aPorA;  b = bPorA;
    else:  % dacă 'a' şi 'b' sunt date implicit, ca valori unghiulare (direcţii)
        a = dir(aPorA);  % 'a'  şi 'b' sunt versorii direcţiilor indicate  
        b = dir(bPorA);
    fi
    (0,0) transformed T = c;         
    (1,0) transformed T = b + c;
    (0,1) transformed T = a + c;
    T  % va transforma y=x^2 în z(t)=at^2 + bt + c (coeficienţi complecşi)
enddef;

def drawpaths = 
    p1 = p0 transformed T;
    draw p0 scaled um withpen pencircle scaled 1 dashed evenly;
    draw p1 scaled um withpen pencircle scaled 1;
enddef;

um = 1in;  % unitatea de măsură (la trasarea pe pagină a contururilor)

beginfig(2);
    save a, b, c, T, p;  pair a, b, c;  path p[];  transform T;
    T = affinity(120, 60, (0.5,0.75));  % arg(a)=120°; arg(b)=60°; c=.5+.75i
    p0 = parstd(-1.2, 0.8);  % conturul parabolei standard
    drawpaths;
    c = (0,0) transformed T;
    b + c = (1,0) transformed T;  % show(b);
    a + c = (0,1) transformed T;  % show(a);
    drawoptions(withpen pencircle scaled 0.1);
    drawarrow (c -- 0.9[c,c+a]) scaled um;
    drawarrow (c -- 0.75[c,c+b]) scaled um;
    drawarrow (origin -- right) scaled um;
    drawarrow (origin -- up) scaled um;
    label.rt(btex $\boldsymbol{c}=z(0)$ etex, c scaled um);
    label.lft(btex $\boldsymbol{a}$ etex, .45[c,c+a] scaled um);
    label.rt(btex $\boldsymbol{b}=z'(0)$ etex, .4[c,c+b] scaled um);
    label.bot(btex ${O}$ etex, origin scaled um);
    label.bot(btex ${x\,(=t)}$ etex, right scaled um);
    label.lft(btex ${y}$ etex, up scaled um);        
    label.bot(btex $y=x^2$ etex rotated 38, (0.6,0.46) scaled um) withcolor .3white;
    label.bot(btex $z(t)=at^2+bt+c$ etex rotated -22, (-0.3,1.12) scaled um) withcolor .3white;
    z0 = (b+c) rotatedaround(c, 180);
    z1 = 0.25[c, z0];
    draw (c -- z1) scaled um;
    currentpicture := currentpicture shifted(2um, 2um);
endfig;

end

%%% compilare (Ubuntu-Linux):
% mpost -tex=latex parabola.mp

%%% transformare ulterioară în PDF:
% ps2pdf parabola.2

%%% obţine BoundingBox pentru figura creată:
% gs -q -o /dev/null -sDEVICE=bbox parabola.2.pdf
%%% şi apoi înscrie CropBox în fişierul PDF:
% gs -q -sDEVICE=pdfwrite -o affine.pdf -f parabola.2.pdf -c "[/CropBox [57 131 236 289] /PAGES pdfmark"

%%% apoi, pentru a include figura într-un document produs cu LaTeX:
% \begin{center}
%     \includegraphics[scale=0.75]{affine.pdf}
% \end{center}



