
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
#let product-title = "IGB Resumo de Dados do País" 
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
      #set par(leading: .2em)

      #let padding = box(width: .5em)
      #let padding-small = box(width: .25em)
      #let icon-legend(key, key-text) = {
        text(key)
        padding-small
        text(key-text)
        padding
      }

      Ícones indicam como os números #h(1fr) \
      se comparam à distribuição global:
      #box(width: 2em)
      #icon-legend("top10", "Os 10% mais altos de todos os países")
      #icon-legend("top33", "Terço superior")
      #icon-legend("mid33", "Terço médio")
      #icon-legend("bot33", "Terço inferior")
      #icon-legend("bot10", "Os 10% mais baixos")
      #h(1fr)
      #counter(page).display("1")
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
          Cite como: International Organization for Migration, Global Data Institute. #date-ymd #text(style: "italic")[GDI Country Data Snapshot: #title.]
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
  
  let figure = text(style: "italic", fill: color-subtle)[→ Figura]
  let figures = text(style: "italic", fill: color-subtle)[→ Figuras]
  let nbox(body) = {
    set text(size: .65em, weight: "bold", fill: white)
    box(
      fill: color-number, 
      inset: (x: 1.5pt, y: .9pt),
      outset: (y: 1.5pt)
    )[#body]
  }

  grid(
    columns: (35%, 1%, 1fr),

    block(fill: color-light, inset: 1.1em)[

      == SOBRE
      
      #block[
        #pad(bottom: 3pt)[

          #block[
            #set text(size: fontsize-med, fill: color-dark)
            #set par(leading: .5em)
            
            Este #text(weight: "bold")[Resumo de Dados do País] é uma breve visão geral de estatísticas relacionadas com a migração, provenientes de conjuntos de dados globais compilados por agências da ONU e outras organizações internacionais.
          ]

          #block[
            #set text(size: fontsize)
            #set par(leading: .35em)
            #set block(spacing: .85em)

            Os mapas são apenas para fins ilustrativos. As fronteiras e os nomes apresentados e as designações utilizadas nos mapas não implicam endosso ou aceitação oficial por parte da OIM. 

            Os dados estão sujeitos às limitações inerentes aos conjuntos de dados globais e devem ser considerados como linhas de base estatísticas, especialmente na ausência de dados locais mais precisos. Os números podem diferir dos publicados por serviços nacionais de estatística ou outras fontes.
          ]
        ]
      ]
    ],

    none,

    block(inset: .8em)[

      == CONTEÚDO
      
      #pad(bottom: 3pt)[
        #block[
          #set text(size: fontsize-med, fill: color-main)
          #set par(leading: .5em)
          #set block(spacing: 1em)

          - Quantos emigrantes e imigrantes existem? Quais são os seus destinos e origens? Qual a sua estrutura etária e de sexo? #figures #nbox[1] #nbox[2] #nbox[3]

          - Quais são as tendências e padrões da migração líquida? #figures #nbox[4] #nbox[5]

          - Quando e onde ocorreram deslocamentos? Quais são as causas? #figures #nbox[6] #nbox[7]

          - Quando e onde migrantes morreram ou desapareceram? #figures #nbox[8] #nbox[9] #nbox[10]

          - Quantos refugiados existem? #figure #nbox[11]

          - Qual o volume das remessas e do investimento direto estrangeiro? #figures #nbox[12] #nbox[13]

          - Quais são as características demográficas da população em geral? #figures #nbox[14] #nbox[15] #nbox[16]

          - Como se desenvolveram os indicadores económicos? #figures #nbox[17] #nbox[18] #nbox[19]
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

    Este Resumo de Dados do País foi preparado pelo Instituto Global de Dados (IGD) da Organização Internacional para as Migrações (OIM) usando o sistema de publicação Quarto. Os gráficos e legendas foram gerados usando R e a paginação foi feita usando Typst. Para perguntas e sugestões, entre em contacto com datasnapshots\@iomint.onmicrosoft.com. #text(size: .75em, fill: color-fade)[v. #version]
  ]
}
