"""Generate the 'PCA by hand' poster (poster.html) -- pure Python, no dependencies.
Render to PDF with:  chromium --headless --no-sandbox --print-to-pdf=pca_poster.pdf poster.html
"""
from math import sqrt

X = [("A", 0, 1), ("B", 1, 2), ("C", 3, 3), ("D", 4, 2)]
n = len(X)
mx = sum(p[1] for p in X) / n
my = sum(p[2] for p in X) / n
C = [(l, x - mx, y - my) for l, x, y in X]
Sxx = sum(a * a for _, a, b in C)
Syy = sum(b * b for _, a, b in C)
Sxy = sum(a * b for _, a, b in C)
assert (mx, my, Sxx, Syy, Sxy) == (2, 2, 10, 2, 3)
r10 = sqrt(10)
S1 = [(l, (3 * a + b) / r10, (a - 3 * b) / r10) for l, a, b in C]
assert abs(sum(s[1] ** 2 for s in S1) - 11) < 1e-9 and abs(sum(s[2] ** 2 for s in S1) - 1) < 1e-9

f = lambda v: f"{v:.2f}".replace("-", "−")
i = lambda v: str(int(v)).replace("-", "−")

def plot(points, axes, lim=3.2, size=300, proj=None, labels=("x", "y")):
    """points: [(label,x,y)], axes: [(dx,dy,color,name)], proj: axis index to drop projections on."""
    s = size / (2 * lim)
    T = lambda x, y: (size / 2 + x * s, size / 2 - y * s)
    o = [f'<svg viewBox="0 0 {size} {size}" class="plot">']
    for k in range(-3, 4):
        a, b = T(k, -lim); c, d = T(k, lim)
        o.append(f'<line x1="{a}" y1="{b}" x2="{c}" y2="{d}" class="grid"/>')
        a, b = T(-lim, k); c, d = T(lim, k)
        o.append(f'<line x1="{a}" y1="{b}" x2="{c}" y2="{d}" class="grid"/>')
    a, b = T(-lim, 0); c, d = T(lim, 0)
    o.append(f'<line x1="{a}" y1="{b}" x2="{c}" y2="{d}" class="ax"/>')
    a, b = T(0, -lim); c, d = T(0, lim)
    o.append(f'<line x1="{a}" y1="{b}" x2="{c}" y2="{d}" class="ax"/>')
    o.append(f'<text x="{size-6}" y="{size/2-6}" class="axl" text-anchor="end">{labels[0]}</text>')
    o.append(f'<text x="{size/2+6}" y="14" class="axl">{labels[1]}</text>')
    for dx, dy, col, name in axes:
        a, b = T(-dx * lim * .88, -dy * lim * .88); c, d = T(dx * lim * .88, dy * lim * .88)
        o.append(f'<line x1="{a}" y1="{b}" x2="{c}" y2="{d}" stroke="{col}" stroke-width="3"/>')
        o.append(f'<text x="{c}" y="{d-6}" fill="{col}" class="pcl" text-anchor="end">{name}</text>')
    if proj is not None:
        dx, dy = axes[proj][:2]
        for _, x, y in points:
            t = x * dx + y * dy
            a, b = T(x, y); c, d = T(t * dx, t * dy)
            o.append(f'<line x1="{a}" y1="{b}" x2="{c}" y2="{d}" class="proj"/>')
    for l, x, y in points:
        a, b = T(x, y)
        o.append(f'<circle cx="{a}" cy="{b}" r="7" class="pt"/><text x="{a+10}" y="{b-9}" class="ptl">{l}</text>')
    o.append('</svg>')
    return "".join(o)

RED, BLUE = "#c0392b", "#1f6fb2"
plot1 = plot(C, [(3 / r10, 1 / r10, RED, "PC1"), (1 / r10, -3 / r10, BLUE, "PC2")], proj=0)
plot2 = plot(S1, [(1, 0, RED, "PC1"), (0, 1, BLUE, "PC2")], lim=3.2, labels=("", ""))

rows_data = "".join(f"<tr><td>{l}</td><td>{x}</td><td>{y}</td></tr>" for l, x, y in X)
rows_cov = "".join(
    f"<tr><td>{l}</td><td>{x}</td><td>{y}</td><td class=c>{i(a)}</td><td class=c>{i(b)}</td>"
    f"<td>{i(a*a)}</td><td>{i(b*b)}</td><td>{i(a*b)}</td></tr>"
    for (l, x, y), (_, a, b) in zip(X, C))
rows_sc = "".join(f"<tr><td>{l}</td><td>{f(p)}</td><td>{f(q)}</td></tr>" for l, p, q in S1)

html = f"""<!doctype html><html><head><meta charset="utf-8"><title>PCA by hand</title>
<style>
@page {{ size: 420mm 297mm; margin: 0 }}
* {{ box-sizing: border-box }}
body {{ margin:0; width:420mm; height:297mm; background:#fbf7ee; color:#222;
  font-family: Georgia, 'Times New Roman', serif; font-size: 15px; padding: 10mm 14mm; overflow:hidden; }}
h1 {{ margin:0; font-size: 44px; letter-spacing:.5px }}
.sub {{ font-size: 19px; color:#555; margin: 2px 0 10px; font-style: italic }}
.grid3 {{ display:grid; grid-template-columns: 1.45fr 1fr 1fr; gap: 12px; }}
.col {{ display:flex; flex-direction:column; gap:12px }}
.box {{ background:#fff; border:2.5px solid #333; border-radius:10px; padding: 11px 15px 12px;
  box-shadow: 4px 4px 0 #d9cfb8 }}
.box h2 {{ margin:0 0 8px; font-size: 21px; display:flex; align-items:center; gap:10px }}
.num {{ background:#333; color:#fff; border-radius:50%; width:30px; height:30px; display:inline-flex;
  align-items:center; justify-content:center; font-size:18px; flex:none }}
table {{ border-collapse: collapse; margin: 6px auto; font-family: 'Courier New', monospace; font-size: 16px }}
th, td {{ border: 1.5px solid #888; padding: 4px 9px; text-align:center }}
th {{ background:#efe6d0 }}
td.c {{ background:#eef5fb }}
tr.sum td {{ font-weight:bold; background:#fff3c4 }}
.eq {{ font-family: 'Courier New', monospace; font-size: 17px; line-height: 1.55; margin: 4px 0 }}
.note {{ font-size: 13.5px; color:#555; margin-top: 5px }}
.hl {{ background:#fff3c4; padding:0 4px; border-radius:3px }}
.mat {{ display:inline-block; vertical-align:middle; border-left:2.5px solid #222; border-right:2.5px solid #222;
  border-radius:8px; padding: 2px 10px; margin: 0 4px; text-align:center; font-family:'Courier New',monospace }}
.mat span {{ display:inline-block; min-width: 46px }}
.big {{ font-size: 19px }}
svg.plot {{ width: 100%; max-width: 235px; display:block; margin: 4px auto; background:#fffdf7; border:1.5px solid #999 }}
.grid {{ stroke:#e6dfcf; stroke-width:1 }} .ax {{ stroke:#666; stroke-width:1.5 }}
.axl {{ font: italic 15px Georgia; fill:#555 }} .pcl {{ font: bold 16px Georgia }}
.pt {{ fill:#222 }} .ptl {{ font: bold 15px Georgia; fill:#222 }}
.proj {{ stroke:#999; stroke-dasharray:4 3; stroke-width:1.5 }}
.bar {{ display:flex; height: 30px; border:2px solid #333; border-radius:6px; overflow:hidden; margin-top:8px; color:#fff; font: bold 15px Georgia }}
.bar div {{ display:flex; align-items:center; justify-content:center }}
.foot {{ margin-top:12px; display:grid; grid-template-columns: 1.6fr 1fr; gap:14px; }}
.hist {{ font-size: 14.5px; line-height: 1.5 }}
.check {{ color:#1e8449; font-weight:bold }}
</style></head><body>
<h1>Principal Component Analysis — by hand</h1>
<div class="sub">Four points, two variables, one pencil. No computer needed.</div>
<div class="grid3">

<div class="col">
 <div class="box"><h2><span class="num">1</span>Data &amp; means</h2>
  <div style="display:flex; gap:24px; align-items:center; justify-content:center">
   <table><tr><th></th><th>x</th><th>y</th></tr>{rows_data}
    <tr class="sum"><td>Σ</td><td>8</td><td>8</td></tr></table>
   <div class="eq">x̄ = 8 / 4 = <b>2</b><br>ȳ = 8 / 4 = <b>2</b></div>
  </div>
 </div>

 <div class="box"><h2><span class="num">2</span>Centre the data &amp; build the covariance matrix</h2>
  <table>
   <tr><th rowspan=2>obs</th><th rowspan=2>x</th><th rowspan=2>y</th><th>x − x̄</th><th>y − ȳ</th><th colspan=3>products</th></tr>
   <tr><th>a</th><th>b</th><th>a·a</th><th>b·b</th><th>a·b</th></tr>
   {rows_cov}
   <tr class="sum"><td>Σ</td><td>8</td><td>8</td><td>0</td><td>0</td><td>{i(Sxx)}</td><td>{i(Syy)}</td><td>{i(Sxy)}</td></tr>
  </table>
  <div class="note"><span class="check">✓ check:</span> the centred columns must sum to 0.</div>
  <div class="eq">Var(x) = Σa² / (n−1) = 10/3 <br>Var(y) = Σb² / (n−1) = 2/3<br>Cov(x,y) = Σab / (n−1) = 3/3 = 1</div>
  <div class="big" style="text-align:center; margin-top:6px">
   C = <span class="mat"><span>10/3</span><span>1</span><br><span>1</span><span>2/3</span></span>
   = ⅓ <span class="mat"><span>10</span><span>3</span><br><span>3</span><span>2</span></span> = ⅓ S</div>
  <div class="note">Shortcut: S = XᵀX for the centred data matrix X. Dividing by n−1 (not n) is “Bessel’s correction”;
  it only rescales the eigenvalues, never the eigenvectors. Work with S, divide by 3 at the end.</div>
 </div>
</div>

<div class="col">
 <div class="box"><h2><span class="num">3</span>Eigenvalues</h2>
  <div class="eq">det(S − λI) = 0<br>(10−λ)(2−λ) − 3·3 = 0<br>λ² − 12λ + 11 = 0<br>(λ − 11)(λ − 1) = 0</div>
  <div class="big" style="text-align:center"><span class="hl">λ₁ = 11</span> &nbsp; <span class="hl">λ₂ = 1</span></div>
  <div class="note"><span class="check">✓</span> trace 10+2 = 12 = 11+1 &nbsp;·&nbsp; det 20−9 = 11 = 11·1<br>
  (of C: 11/3 and 1/3)</div>
 </div>
 <div class="box"><h2><span class="num">4</span>Eigenvectors</h2>
  <div class="eq">λ = 11: (S − 11I)v = 0<br>
   −v₁ + 3v₂ = 0 → v = (3, 1)<br>length √10 → <b style="color:{RED}">PC1 = (3, 1)/√10</b><br><br>
   λ = 1: 9v₁ + 3v₂ = 0 → v = (1, −3)<br><b style="color:{BLUE}">PC2 = (1, −3)/√10</b></div>
  <div class="note"><span class="check">✓</span> perpendicular: 3·1 + 1·(−3) = 0</div>
  {plot1}
  <div class="note" style="text-align:center">Centred data with the two principal axes. Dashed: projection onto PC1.</div>
 </div>
</div>

<div class="col">
 <div class="box"><h2><span class="num">5</span>Scores (project the data)</h2>
  <div class="eq">PC1 = (3a + b)/√10<br>PC2 = (a − 3b)/√10</div>
  <table><tr><th>obs</th><th>PC1</th><th>PC2</th></tr>{rows_sc}
   <tr class="sum"><td>Σ score²</td><td>11</td><td>1</td></tr></table>
  <div class="note"><span class="check">✓</span> Σ score² = eigenvalue: (49+9+16+36)/10 = 11 and (1+1+4+4)/10 = 1</div>
  {plot2}
  <div class="note" style="text-align:center">Same points, rotated into the PC1–PC2 frame.</div>
 </div>
 <div class="box"><h2><span class="num">6</span>Variance explained</h2>
  <div class="eq">PC1: 11 / (11+1) = <b>91.7 %</b> &nbsp; PC2: 1/12 = <b>8.3 %</b></div>
  <div class="bar"><div style="width:91.7%;background:{RED}">PC1 91.7 %</div><div style="width:8.3%;background:{BLUE}"></div></div>
  <div class="note">One axis keeps almost all the information: 2-D → 1-D.</div>
 </div>
</div>
</div>

<div class="foot">
 <div class="box hist"><b>A little history.</b> Karl Pearson (1901) posed it geometrically as the “line of closest fit” to points in space;
 Harold Hotelling (1933) named the “principal components” and computed them with an iterative power method on a desk calculator.
 Both did precisely what this poster does — sums of squares and products, then a characteristic equation — by hand.</div>
 <div class="box hist"><b>Recipe.</b> centre → covariance → eigen-decomposition → sort by λ → project.<br>
 Data: A(0,1) B(1,2) C(3,3) D(4,2) — numbers chosen so the eigenvalues (11, 1) are whole.</div>
</div>
</body></html>"""
open("poster.html", "w", encoding="utf-8").write(html)
print("wrote poster.html")
