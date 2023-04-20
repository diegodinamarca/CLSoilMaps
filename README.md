# CLSoilMaps
CLSoilMaps is a database of maps of soil physical and hydraulic properties for Chile. This is and on going project for creating maps of soil properties using
digital soil mapping techniques.

Maps of soil phyisical properties were produced using a Random Forest Model trained with more than 4000 soil observations.
Soil hydraulic properties were modelled using a pedotransfer function (Rosetta V3). 
All soil properties were predicted at six standards depths: 0-5, 5-15, 15-30, 30-60, 60-100, 100-200 cm following GlobalSoilMaps project specifications.
The resulting maps have a resolution of 0.01° (near 100m).

## Properties modeled
| Soil Attribute                 | File Abbreviation | Description                              | Units   |
|--------------------------------|-------------------|------------------------------------------|---------|
| Bulk density                   | Bulkd             | Bulk density of the fine fraction        | g/cm3   |
| Clay                           | Clay              | Clay content                             | %       |
| Sand                           | Sand              | Sand content                             | %       |
| Silt                           | Silt              | Silt content                             | %       |
| Field Capacity                 | FC                | Field capacity at 330 kPa                | cm3/cm3 |
| Permanent Wilting Point        | PWP               | Permanent wilting point at 15000 kPa     | cm3/cm3 |
| Available Water Capacity       | AWC               | Available water capacity as 100*(FC-PWP) | mm    |
| Total Available Water Capacity | Total_AWC         | Sum of AWC across all depths             | mm      |
| Available Moisture             | AvMoist           | Available Moisture as FC-PWP             | cm3/cm3 |
| Residual water content         | theta_r           | Residual water content                   | cm3/cm3 |
| Saturated water content        | theta_s           | Saturated water content                  | cm3/cm3 |
| Alpha shape parameter          | alpha             | "alpha" shape parameter                  | 1/cm    |
| N shape parameter              | n                 | "n" shape parameter                      | -       |
| Soil Hydraulic Conductivity    | ksat              | Saturated hydraulic conductivity         | cm/day  |

For more information you can contact me at ddinamarcamuller@gmail.com
