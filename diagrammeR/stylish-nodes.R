#----------------------------------------------------------------------------------------------
# http://www.buildingwidgets.com/blog/2015/1/8/week-01-easy-flowcharts-and-diagrams-diagrammer
#----------------------------------------------------------------------------------------------

library(DiagrammeR)

# stylish nodes
DiagrammeR("
  graph TD;
           A[rect]-- add style -->A2[rect + style];
           B{rhombus}---|+ some style|B2{rhombus + style};
           C(rounded);   D((circle)); 
           style A2 fill:#c12,stroke-width:5px;
           style B2 fill:none, stroke-dasharray:10;
           ")


# workflow of a post graph
DiagrammeR("
           graph TD;
           hw{htmlwidget} -->experiments;
           hw -->content;
           experiments -->RMarkdown
           content -->RMarkdown
           RMarkdown -->|R/knitr|Markdown;
           Markdown -->|Pandoc|HTML;
           HTML -->|git push| Github;
           HTML -->|copy/paste| Squarespace ;
           ")