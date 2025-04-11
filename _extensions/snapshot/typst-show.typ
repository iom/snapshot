#show: doc => article(

  $if(title)$
    title: [$title$],
  $endif$
  
  $if(iso)$
    iso: [$iso$],
  $endif$
  
  $if(version)$
    version: [$version$],
  $endif$
  
  doc,
)