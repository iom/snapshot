
#let b(body) = {
  set text(weight: "bold", fill: rgb("#0033A0"))
  [#body]
}

#show link: underline

//// About

#let about(body) = {
  set text(size: .7em)
  set par(leading: .575em)
  pad(bottom: 3pt, body)
}

#let endnote(body) = {
  set par(leading: .5em, justify: true)
  set text(size: .5em, fill: rgb("#8099D0"))
  [#body]
}

//// Headers

#let h0(body) = {
  set text(size: .7em)
  rect(width: auto, height: .4cm, inset: 2pt, outset: 1pt, fill: rgb("#0033A0"))[
    #text(weight: "bold", fill: white, baseline: 1pt, body)
  ]
}

#let h1(head) = {
  set text(size: .7em)
  grid(
    rows: (.4cm, .025cm),
    columns: (auto, 1fr),
    rect(width: auto, height: 100%, inset: 2pt, outset: 1pt, fill: rgb("#0033A0"))[
      #text(weight: "bold", fill: white, baseline: 1pt, head)
    ],
    none,
    rect(width: 100%, height: 100%, outset: 1pt, fill: rgb("#0033A0")),
    rect(width: 100%, height: 100%, outset: 1pt, fill: rgb("#0033A0"))
  )
}

#let h2(head, subhead) = {
  set text(size: .7em)
  grid(
    columns: (auto, 1fr),
    rows: (.4cm, .025cm),
    rect(width: auto, height: 100%, inset: 2pt, outset: 1pt, fill: rgb("#0033A0"))[
      #text(weight: "bold", fill: white, baseline: 1pt, head)
    ],
    rect[#text(fill: rgb("#4068B8"), size: .65em, baseline: 1pt, subhead)],
    rect(outset: 1pt, fill: rgb("#0033A0")),
    none
  )
}

//// Table of contents

#let contents(body) = {
  set text(size: .7em)
  set par(leading: .50em)
  set block(spacing: 1em)
  pad(bottom: 3pt, body)
}

#let figure(body) = {
  set text(style: "italic", fill: rgb("#84ADEC"))
  [#body]
}

#let nbox(body) = {
  set text(size: .65em, weight: "bold", fill: white)
  box(
    fill: rgb("#ADC9F2"), 
    inset: (x: 1.5pt, y: .9pt),
    outset: (y: 1.5pt)
  )[#body]
}

//// Captions

#let caption1(body) = {
  set text(size: .65em, fill: rgb("#4068B8"))
  set par(leading: .525em)
  box(
    width: 100%, 
    // fill: rgb("#f2f7fd"),
    fill: rgb("#f4f8fd"),
    // fill: none,
    inset: .8em,
    stroke: (top: 1.5pt + rgb("#CEDEF7"), rest: none),
    body
  )
}

#let caption(body) = {
  set text(size: .65em, fill: rgb("#4068B8"))
  set par(leading: .525em)
  box(
    // fill: rgb("#f2f7fd"),
    fill: rgb("#f4f8fd"),
    // fill: none,
    inset: .8em,
    stroke: (top: 1.5pt + rgb("#CEDEF7"), rest: none),
    columns(2, gutter: 2.5em)[#body]
  )
}

#let caption3(body) = {
  set text(size: .65em, fill: rgb("#4068B8"))
  set par(leading: .525em)
  box(
    // fill: rgb("#f2f7fd"),
    fill: rgb("#f4f8fd"),
    // fill: none,
    inset: .8em,
    stroke: (top: 1.5pt + rgb("#CEDEF7"), rest: none),
    columns(3, gutter: 1.8em)[#body]
  )
}
