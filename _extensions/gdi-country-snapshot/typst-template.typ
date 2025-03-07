
#let article(
  title: none,
  iso: none,
  datayear: 2025,
  datamonth: 1,
  dataday: 27,
  version: none,
  logo: "./inst/gdi-logo-white/GDILOGO.png",
  cols: 1,
  margin: (x: 2cm, y: 2cm),
  paper: "a4",
  lang: "en",
  region: "US",
  font: "Gill Sans Nova",
  fontsize: 11pt,
  sectionnumbering: none,
  toc: false,
  doc,
) = {
  set page(
    paper: paper,
    margin: margin,
    numbering: "1",
    header: context { if(counter(page).get().at(0) > 1) [
        #set align(center)
        #set text(size: .7em, fill: rgb("#8099D0"))
        #grid(
          rows: 1.5cm,
          columns: (auto, 1fr, auto),
          text("GDI COUNTRY DATA SNAPSHOT | " + datetime.today().display("[day padding:none] [month repr:long] [year]")),
          none,
          text(upper(title)),
        )
      ] else [
        none
      ]
    },
    // header: [
    //   #set align(center)
    //   #text(
    //     size: .7em, 
    //     fill: rgb("#8099D0"), 
    //     "FOR RESTRICTED USE ONLY – DRAFT VERSION FOR CONSULTATION"
    //   )
    // ],
    footer: [
      #set align(center)
      #set text(9pt)
      #text(
        size: .7em, 
        fill: rgb("#8099D0"), 
        "Icons indicate how numbers compare to the global distribution:     " + 
          box(width: 5pt, image("./inst/top10.png")) + "  Top 10% of all countries     " + 
          box(width: 5pt, image("./inst/top33.png")) + "  Top third     " + 
          box(width: 5pt, image("./inst/mid33.png")) + "  Middle third     " + 
          box(width: 5pt, image("./inst/bot33.png")) + "  Bottom third     " + 
          box(width: 5pt, image("./inst/bot10.png")) + "  Bottom 10%"
      )
    ]
  )

  set par(justify: true)
  set text(lang: lang,
           region: region,
           font: font,
           size: fontsize,
           fill: rgb("#0033A0"))
  set heading(numbering: sectionnumbering)
  
  //let howtoread(body) = {
  //  set align(center)
  //  set par(leading: .5em, justify: true)
  //  set text(size: .7em, fill: rgb("#4068B8"))
  //  [#body]
  //}
  
  // Title header

  // page(background: place(left + top, rect(
  //     fill: rgb("#0033A0"),
  //     width: 100%,
  //     height: 100pt,
  //   ))
  // )[]

  place(left + top, dx: -2cm, dy: -2cm, rect(
    fill: rgb("#0033A0"),
    width: 21cm,
    height: 3.5cm,
  ))
  
  place(left + top, dx: -.75cm, dy: -.75cm, float: true,
    grid(
      rows: (1.65cm),
      columns: (14.75cm, 4.25cm),
      row-gutter: .25cm,
      rect(width: 100%, height: 100%, inset: 7pt, fill: none, stroke: none)[
        #text(tracking: .5pt, size: .75em, fill: rgb("#B3C2E3"), "GDI COUNTRY DATA SNAPSHOT | " + datetime.today().display("[day padding:none] [month repr:long] [year]")) \
        #text(weight: "bold", size: 1.75em, fill: white, upper(title)) \
        #text(size: .6em, fill: rgb("#8099D0"), "Cite as:")
          #text(size: .6em, fill: rgb("#8099D0"), "International Organization for Migration, Global Data Institute.") 
          #text(size: .6em, fill: rgb("#8099D0"), datetime.today().display("[year], [month repr:long] [day padding:none]."))
          #text(size: .6em, style: "italic", fill: rgb("#8099D0"), "GDI Country Data Snapshot: " + title + ".")
      ],
      rect(width: 100%, height: 100%, fill: none, stroke: none)[
        #align(center + top, image(logo, width: 4.25cm))
      ]
    )
  )

  // align(left,
  //   grid(
  //     rows: (1.65cm),
  //     columns: (.25cm, 1fr, 4cm),
  //     row-gutter: 2pt,
  //     rect(width: 100%, height: 100%, fill: rgb("#0033A0")),
  //     rect(width: 100%, height: 100%, inset: 7pt, fill: rgb("#D9E0F1"))[
  //       #text(tracking: .5pt, size: .75em, fill: rgb("#0033A0"), "GDI COUNTRY DATA SNAPSHOT") \
  //       #text(weight: "bold", size: 1.25em, fill: rgb("#0033A0"), upper(title)) \
  //       #text(size: .5em, fill: rgb("#8099D0"), "Data last updated on " + datetime(year: datayear, month: datamonth, day: dataday).display("[day padding:none] [month repr:long] [year]."))
  //     ],
  //     rect(width: 100%, height: 100%, fill: rgb("#0033A0"))[
  //       #align(center + horizon, image(logo, width: 3.5cm))
  //     ]
  //   )
  // )
  
  doc
  
}
