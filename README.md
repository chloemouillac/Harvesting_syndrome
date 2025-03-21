This is the code linked to the research paper "Are wild-harvested plants just ordinary? Traits and harvesting patterns in France."

All of the code used to perform the analysis and partial data are available here.

To run the code, refer to the list of data needed below.

In order to run the code correctly, every folder in the project (0_List_vascular_flora through 5_Conservation_status) should be organised in subfolders (raw_data, processed_data and plots).

DATA AVAILABILITY :

Lists of species:
The list of harvested species used for the analysis comes from:
Lescure, J.-P., Thevenin, T., Garreta, R., Morisson, B., 2015. Les plantes faisant l’objet de cueillettes commerciales sur le territoire métropolitain. Une liste commentée. Monde Plantes 19–39.
We removed algae, mushrooms, lichen and moss. The final list can be found at https://github.com/chloemouillac/Harvesting_syndrome under the name liste_Lescure_v17.csv.

The list of vascular flora used for the analysis is an update of the 2018 French Red List for plant species and can be found at https://github.com/chloemouillac/Harvesting_syndrome under the name list_vascular_v17.csv.

We used Taxref V17 as our References for taxonomy.
References : 
Gargominy, O., 2024. TAXREF v17.0, référentiel taxonomique pour la France.
Links for download : 
https://inpn.mnhn.fr/telechargement/referentielEspece/taxref/17.0/menu

The correspondence table we used to harmonise taxonomy between the different datasets used for the analysis can be found at  https://github.com/chloemouillac/Harvesting_syndrome under the name WHP_correpondence_table_v17.csv.


Occurrence data:
Occurrence data can be found in Open Obs and GBIF.
References :
Open Obs extraction on 21/09/2023
GBIF.org (25 July 2024) GBIF Occurrence Download https://doi.org/10.15468/dl.z8mvmj
Links for download : 
https://openobs.mnhn.fr/
https://doi.org/10.15468/dl.z8mvmj
For details on filtering and extraction steps, you can refer to the R code (found at https://github.com/chloemouillac/Harvesting_syndrome, in 2_Distribution).


Ethnobotanical data:
Plant uses come from the PFAF (Plants For A Future) website and Kew's World Checklist of Useful Plant Species. We also used a 1999 synthesis detailing the uses of plants in the area covered by the Mediterranean Botanical Conservatory area (Chaber and Lieutaghi, unpublished, contact k.diadema@cbnmed.fr).
References: 
PFAF, 2024. https://pfaf.org/user/ 
Diazgranados, M., Allkin, B., Black, N., Cámara-Leret, R., Canteiro, C., Carretero, J., Eastwood, R., Hargreaves, S., Hudson, A., Milliken, W., Nesbitt, M., Ondo, I., Patmore, K., Pironon, S., Turner, R., Ulian, T., 2020. World checklist of useful plant species. https://doi.org/10.5063/F1CV4G34


Harvested plant parts come from PFAF, the SICARAPPAM’s selling lists (France’s largest WHP cooperative in the Massif-Central). We also included plant lists from a 2020 report on wild harvesting in the Ardèche department (Muraz and PNR Monts d’Ardèche, 2018) and unpublished data from the National Botanical Conservatories (contact contact@fcbn.fr for access).
References: 
PFAF, 2024. https://pfaf.org/user/ 
Coopérative agricole de producteurs auvergne - Sicarappam [WWW Document], 2024. . Sicarappam. URL https://www.sicarappam.com/ (accessed 8.22.24).
Muraz, M., PNR Monts d’Ardèche, 2018. La cueillette commerciale de plantes sauvages sur les monts d’Ardèche. Mémoire ingénieur agronome.


Biological data:
Trait data came from TRY (request 36998), and the DivGrass database (Violle C., Borgy B., and Choler P., unpublished, contact cyrille.violle@cefe.cnrs.fr for access).
References: 
Kattge, J, Bönisch, G, Díaz, S, et al. TRY plant trait database – enhanced coverage and open access. Glob Change Biol. 2020; 26: 119– 188. https://doi.org/10.1111/gcb.14904


Links for download: 
https://www.try-db.org/TRY
Final CSR calculations can be found in Appendix H.
Raunkiaer life forms can be found in the Baseflor and in the Flore de la France méditerranéenne continentale.
References: 
Tison, J.-M., Foucault, B. de (Eds.), 2014. Flora Gallica: flore de France. Biotope éditions, Mèze, Hérault.
Julve, P., 1998. Baseflor. Index botanique, écologique et chorologique de la flore de France
Links for download: 
https://www.researchgate.net/publication/302925607_baseflor


Conservation data:
Conservation data (protections, regulations and IUCN status’) can be found in the Base de connaissances “Statuts”.
References : 
Gargominy, O., Régnier, C., PatriNat (OFB-MNHN-CNRS-IRD), 2024. Base de connaissances “Statuts” des espèces en France.
Links for download :
https://inpn.mnhn.fr/telechargement/referentielEspece/bdc-statuts-especes
