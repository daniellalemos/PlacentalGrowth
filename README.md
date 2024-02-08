<img src='App/www/minho.png' width='150'>

# Nonlinear Regression on Growth Curves for Placental Parameters in R
This repository contains the code and resources related to the master's thesis in Bioinformatics, titled "Nonlinear Regression on Growth Curves for Placental Parameters in R". In this study, the applicability of nonlinear regression models on growth curves of placental and fetal parameters was explored using a dataset of Portuguese parturients provided by CGC Genetics, Porto.

## Summary
In recent years, there has been a growing interest in evaluating biometric parameters of the placenta and their relationship with obstetric outcomes. Evidence suggests that placental measurements and their evolution may reflect changes in fetal development and even newborn and adult diseases. In this context, the main objective of this study was to construct reference growth curves for parameters such as Diameter 1 (D1) and 2 (D2), Placental Thickness (PT), Placental Weight (PW), and Fetal Weight (FW) using a semiparametric regression approach.

## Repository Contents

The repository is organized as follows:

- **App**: Contains files related to the web application developed to analyze and interpret the constructed growth curves.

    - Files `global.r`, `ui.r`, `server.r` contain the code of the web application.
    - CSV files contain data for plotting the curves: `diameter1.csv`, `diameter2.csv`, `fetalweight.csv`, `placentalweight.csv`, `placentalthickness.csv`.
    - HTML files for the application interface: `home.html`, `intro_text.html`, `ratios.html`.
    - File `help.csv` contains information for the help menu.
    - File `report.rmd` contains the template of the report produced by the application with the analyses.
    - Folder `www` contains additional resources, such as custom CSS style sheets, a user guide in PDF, and PNG images.

- **Growth_Curves**: Contains the code and figures related to the construction of growth curves for different parameters.

    - `Diametro1`, `Diametro2`, `Espessura`, `Peso`, `Peso_fetal`: Subdirectories for each parameter.
        - `Codigo`: Contains files with the used code.
        - `Figuras`: Images resulting from the code.

- **Dados**: Contains the files of the data used in this study and the code related to its processing and exploration.

    - `Codigo`: Contains files with the code for data processing and exploration.
    - `Figuras`: Images resulting from the code.
    - `dataset_placenta.csv`: Original dataset.
    - `dataset_placenta_tratado.csv`: Processed dataset.


## Web Application   

 <img src='App/www/logo_app.png' width='250'>
 
A web application "PlacentalGrowth" was developed to allow healthcare professionals and researchers to analyze and interpret the constructed growth curves. 

The application can be accessed at [placentalgrowth.shinyapps.io/uminho_pt/](https://placentalgrowth.shinyapps.io/uminho_pt/).

## Results and Implications

The results of this study provide important information about placental development and have significant implications for clinical practice in obstetrics, allowing for the advancement and monitoring of maternal-fetal health.

## Author

- Daniela Filipa Machado Lemos
- Contact: pg45469@alunos.uminho.pt

Thesis supervised by: 
- Professor Ana Cristina de Silva Braga, PhD
- Professor Rosete Maria Amorim Novais Nogueira Cardoso, PhD

