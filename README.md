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

The repository contains geospatial data and a geojson file with european country boundaries. You can find the files in the `data` folder. For better performance the resolution is set to low quality by default. To increase the resolution quality change the resolution from low to medium or high in App.js.

## Built with:
- [React Native](https://reactnative.dev)
- [Mapbox GL JS](https://github.com/rnmapbox/maps)
- [Center Of Polygon](https://github.com/mapbox/polylabel)
