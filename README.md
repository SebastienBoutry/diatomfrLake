
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Installation de `{diatomfrlake}`

La version du paquet `{diatomfrlake}` peut se télécharger via le site
Github pour cela on aura besoin du paquet `{remotes}`:

``` r
remotes::install_github("SebastienBoutry/diatomfrlake")
library(diatomlakefr)
```

## Le paquet `{diatomfrlake}`

L’utilisation du protocole d’échantillonage (Morin et al. 2018) permet
de suivre les communautés de diatomées dans les plans d’eau afin de
répondre aux exigences de la Directive Cadre Européenne sur l’eau.

Deux fichiers recueillent les données :

  - la liste floristique dans un format .txt ou .prn sortie du logiciel
    Omnidia (Lecointe, Coste, and Prygiel 1993)
  - les données mésologiques dans un format tableur .ods ou .xls

Le formulaire de saisie est téléchargeable sur le site
[Hydrobio-DCE](https://hydrobio-dce.inrae.fr/) sous deux formats .ods ou
.xls.

Le paquet `{diatomfrlake}` sert à importer les deux fichiers dans le
logiciel R et met en forme les données dans un format tableur. On y
trouve plusieurs fonctions afin d’importer les données jusqu’à la mise
en forme des données.

Afin de placer les unités d’observation potentielles, il est conseillé
d’utiliser le paquet `{lakemetrics}` disponible sur github.

## References:

<div id="refs" class="references">

<div id="ref-Lecointe1993">

Lecointe, C., M. Coste, and J. Prygiel. 1993. “‘Omnidia’: Software for
Taxonomy, Calculation of Diatom Indices and Inventories Management.”
*Hydrobiologia* 269 (1): 509–13. <https://doi.org/10.1007/BF00028048>.

</div>

<div id="ref-Morin2018">

Morin, Soizic, Damien Valade, Juliette Rosebery, and Vincent Bertrin.
2018. “Echantillonnage Des Communautés de Phytobenthos En Plans d’eau.”
Technical report. Irstea.

</div>

</div>
