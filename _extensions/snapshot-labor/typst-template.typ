
// Colors
#let color-dark = rgb("#0033A0")
#let color-main = rgb("#4068B8")
#let color-subtle = rgb("#8099D0")
#let color-fade = rgb("#B3C2E3")
#let color-number = rgb("#ADC9F2")
#let color-stroke = rgb("#CEDEF7")
#let color-light = rgb("#f4f8fd")

// Styles
#let font-family = "Gill Sans Nova"
#let fontsize = 7pt
#let fontsize-med = 8.5pt
#let fontsize-big = 18pt
#let fontsize-small = 6pt
#let b(body) = text(weight: "bold", fill: color-dark)[#body]

// Constants
#let product-title = "GDI Country Data Snapshot: Labour Mobility"
#let date-dmy = datetime.today().display(
  "[day padding:none] [month repr:long] [year]"
)
#let date-ymd = datetime.today().display(
  "[year], [month repr:long] [day padding:none]"
)

// Images
#let logo-path = "./inst/gdi-logo-white/GDILOGO.png"
#let top10 = box(width: 5.5pt, baseline: .5pt, image("./inst/top10.png"))
#let mid33 = box(width: 5.5pt, baseline: .5pt, image("./inst/top33.png"))
#let top33 = box(width: 5.5pt, baseline: .5pt, image("./inst/mid33.png"))
#let bot33 = box(width: 5.5pt, baseline: .5pt, image("./inst/bot33.png"))
#let bot10 = box(width: 5.5pt, baseline: .5pt, image("./inst/bot10.png"))

// Section header

#let header(name: none, note: "") = {
  set rect(height: 100%, outset: 1pt, fill: color-dark)

  grid(
    columns: (auto, 1fr),
    rows: (.4cm, .025cm),

    rect(width: auto, inset: 2pt)[
      #text(
        weight: "bold",
        size: fontsize-med,
        fill: white,
        baseline: 1pt
      )[#upper(name)]
    ],
    rect(fill: none, stroke: none)[
      #text(
        style: "italic",
        size: fontsize,
        fill: color-main,
        baseline: -3pt
      )[#note]
    ],
    grid.cell(colspan: 2, rect(width: 100%))
  )
}

// Caption formatting

#let caption(body, ncol: 2) = {
  set text(size: fontsize, fill: color-main)
  set par(leading: .5em)
  set box(
    width: 100%, 
    fill: color-light,
    inset: .8em,
    stroke: (top: 1.5pt + color-stroke, rest: none)
  )

  box[#columns(ncol, gutter: 2.5em)[#body]]
}

// Template proper

#let article(
  title: none,
  iso: none,
  version: none,
  logo: logo-path,
  doc
) = {

  set par(justify: true)
  set text(
    font: font-family,
    size: fontsize,
    fill: color-subtle
  )

  show "top10": icon => top10
  show "top33": icon => top33
  show "mid33": icon => mid33
  show "bot33": icon => bot33
  show "bot10": icon => bot10

  set page(
    paper: "a4",
    margin: (x: 2cm, y: 2cm),
    numbering: "1",

    // Header shows product name, country, and date
    header: context { 
      if(counter(page).get().at(0) > 1) [
        #set align(center)
        #set text(fill: color-subtle)
        #grid(
          rows: 1.5cm,
          columns: (auto, 1fr, auto),
          text[#upper(product-title) | #date-dmy],
          none,
          text[#upper(title)],
        )
      ] else [
        none
      ]
    },

    // Footer shows explanation of value distribution icons
    footer: [
      #set align(center)
      #set text(fill: color-subtle)

      #let padding = box(width: 1em)
      #let padding-small = box(width: .5em)
      #let icon-legend(key, key-text) = {
        text(key)
        padding-small
        text(key-text)
        padding
      }

      Icons indicate how numbers compare to the global distribution:
      #padding
      #icon-legend("top10", "Top 10% of all countries")
      #icon-legend("top33", "Top third")
      #icon-legend("mid33", "Middle third")
      #icon-legend("bot33", "Bottom third")
      #icon-legend("bot10", "Bottom 10%")
    ]
  )

  // Header styling

  show heading.where(level: 1): it => [
    #set text(size: fontsize-med, baseline: 1pt, weight: "bold", fill: white)
    #set align(left)
    #set block(above: 1em)
    #set rect(height: 100%, outset: 1pt, fill: color-dark)

    #pad(bottom: 5pt)[
      #grid(
        rows: (.4cm, .025cm),
        columns: (auto, 1fr),

        rect(width: auto, inset: 2pt)[#text[#upper(it.body)]],
        none,
        grid.cell(colspan: 2, rect(width: 100%))
      )
    ]
  ]

  show heading.where(level: 2): it => [
    #set text(size: fontsize-med, baseline: 1pt, weight: "bold", fill: white)
    #set align(left)
    #set rect(width: auto, height: .4cm, inset: 2pt, outset: 1pt, fill: color-dark)
    #pad(bottom: 3pt)[#rect[#text[#upper(it.body)]]]
  ]

  // Hero banner

  place(left + top, dx: -2cm, dy: -2cm)[
    #rect(
      fill: color-dark,
      width: 21cm,
      height: 3.5cm,
    )
  ]

  place(left + top, dx: -.75cm, dy: -.75cm, float: true,
    grid(
      rows: (1.65cm),
      columns: (14.75cm, 4.25cm),
      row-gutter: .25cm,
   
      block(inset: 7pt)[

        #let product = block[
          #set text(size: 1.2em, tracking: .5pt, fill: color-fade)
          #upper(product-title) | #date-dmy
        ]

        #let country = block[
          #set text(tracking: .5pt, size: fontsize-big, weight: "bold", fill: white)
          #upper(title)
        ]

        #let citation = block[
          #set text(fill: color-fade)
          Cite as: International Organization for Migration, Global Data Institute. #date-ymd #text(style: "italic")[#product-title: #title.]
        ]

        #product
        #country
        #citation
      ],

      block[
        #image(logo, width: 4.25cm)
      ]
    )
  )

  // About and contents boxes

  grid(
    columns: (35%, 1%, 1fr),

    block(fill: color-light, inset: 1.1em)[

      == ABOUT
      
      #block[
        #pad(bottom: 3pt)[

          #block[
            #set text(size: fontsize-med, fill: color-dark)
            #set par(leading: .5em)
            
            This #text(weight: "bold")[#product-title] is a brief overview of migration-related statistics from global datasets compiled by UN agencies and other international organisations.
          ]

          #block[
            #set text(size: fontsize)
            #set par(leading: .35em)
            #set block(spacing: .85em)

            Maps are for illustration purposes only. The boundaries and names shown and the designations used on maps do not imply official endorsement or acceptance by IOM.
            
            Data are subject to the inherent limitations of global datasets and should be considered as statistical baselines, especially in the absence of more precise local data. Figures may differ from those published by national statistics offices or other sources.
          ]
        ]
      ]
    ],

    none,

    block(inset: .8em)[

      == CONTENTS
      
      #pad(bottom: 3pt)[
        #block[
          #set text(size: fontsize-med, fill: color-main)
          #set par(leading: .5em)
          #set block(spacing: 1em)

          #let figure = text(style: "italic", fill: color-subtle)[→ Figure]
          #let nbox(body) = {
            set text(size: .65em, weight: "bold", fill: white)
            box(
              fill: color-number, 
              inset: (x: 1.5pt, y: .9pt),
              outset: (y: 1.5pt)
            )[#body]
          }

          #b[Demography, employment, and unemployment of immigrants]

          - Where have immigrants come from, and what is their demographic structure? #figure #nbox[1] – #nbox[3]

          - How many foreign- and native-borns are employed, and what are their skill level and sex distributions? #figure #nbox[4] #nbox[5]

          - How do unemployment rates compare between foreign- and native-borns, and how do they vary by skill level and sex? #figure #nbox[6] #nbox[7]

          - What are average working hours and wages of employed foreign- and native-borns? #figure #nbox[8] #nbox[9]

          - Which economic sectors do immigrants work in? #figure #nbox[10]

          #b[Sectoral analysis of national economy]

          -	How do different economic sectors compare in terms of value added and export volume? #figure #nbox[11] #nbox[12]

          -	How are employment and skill levels distributed across economic sectors? #figure #nbox[13]

          -	Which economic sectors did unemployed persons previous work in? #figure #nbox[14]

          -	What is the educational structure and language distribution? #figure #nbox[15] #nbox[16]

          #b[Comparison to origin countries’ economies by sector and skill level]

          -	How does the national skill level distribution compare to that in migrant origin countries and in countries worldwide? #figure #nbox[17] – #nbox[19]

          -	How does the sectoral employment and unemployment distribution compare with that in migrant origin countries and in countries worldwide? #figure #nbox[20] – #nbox[25]
        ]
      ]
    ]
  )
  
  // Body of document

  doc

  // Fine print information

  block(above: 3em)[
    #set par(leading: .5em, justify: true)
    #set text(size: fontsize-small, fill: color-subtle)

    The #product-title was prepared by the Global Data Institute (GDI) of the International Organization for Migration (IOM) using the publishing system Quarto. Charts and captions were generated using R and layouting was done using Typst. For questions and suggestions, contact datasnapshots\@iomint.onmicrosoft.com. #text(size: .75em, fill: color-fade)[v. #version]
  ]
}
