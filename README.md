# Mapbox_UX

The prototype application creates two different map types, either a choropleth or a graduated symbol map to visualize geospatial data on a mobile device. These maps use different colors and different sizes as visual variables to perceive information. 

## Prerequisites

This application is built with React Native. Follow the `React Native CLI Quickstart` installation guide from the official docs.

## Installation Guide

To make the app run, follow the upcoming instructions.

1. Download/Clone this repository.
2. Install dependencies in root folder using npm:
```
npm install
```
3. Run the react-native application:
```
react-native run-android
```

## Geospatial Datasets and Country Boundaries
The repository contains geospatial data and a geojson file with european country boundaries. You can find the files in the `data` folder. For better performance the resolution is set to low quality by default. To increase the resolution change it from low to medium or high in `App.js` in line 6. Country boundaries were downloaded from [https://geojson-maps.ash.ms](https://geojson-maps.ash.ms). The datasets displayed on the maps were open-source and were provided as JSON datasets from the articles at [Our World in Data](https://ourworldindata.org). 

## Map Configuration
The map can be configured in the `data/config.json` file. Here, you can set the visualization type to "graduated_symbol" or "choropleth". If the visualization type is "graduated_symbol", the size distance can be set as "current_constant". If the visualization type is "choropleth", the color distance can be set as "current_color_distance". In either way, in this prototype you can choose between ten different size and color distances. 

## Built with:
- [React Native](https://reactnative.dev)
- [Mapbox GL JS](https://github.com/rnmapbox/maps)
- [Center Of Polygon](https://github.com/mapbox/polylabel)
