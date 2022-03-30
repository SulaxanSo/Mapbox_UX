import React from 'react';
import { StyleSheet, View } from 'react-native';
import MapboxGL from '@react-native-mapbox-gl/maps';
import polylabel from "@mapbox/polylabel";
import * as d3 from "d3";
import { countries } from "./data/countries";
import * as dataset from './data/dataset.json';
import * as config from './data/config.json';


MapboxGL.setAccessToken('pk.eyJ1Ijoic3VsYXhhbjI3IiwiYSI6ImNpc3JqNXRidTAwNHAyeXBiY2hxdnlsMG4ifQ.IGQkafPlHEjq7_hno0AnrA');

const App = () => {
  
  console.log(countries.features.length, "countries are part of the feature collection.");
  
  let points = [];

  // calculate center of polygon
  for(let i = 0, l = countries.features.length; i < l; i++){
    let title = countries.features[i].properties.admin;
    let output = [];

    if (countries.features[i].geometry.type === "Polygon"){
      output = polylabel(countries.features[i].geometry.coordinates);
    }
    else {
      let maxArea = 0, maxPolygon = [];
      for (let j = 0, m = countries.features[i].geometry.coordinates.length; j < m; j++){
        const p = countries.features[i].geometry.coordinates[j];
        const area = d3.geoArea({type: "Polygon", coordinates: p})
        if (area > maxArea){
          maxPolygon = p;
          maxArea = area;
        }
      }
      output = polylabel(maxPolygon);
    }
    //console.log(title, output);
    points.push(<MapboxGL.PointAnnotation key = {i} id={title} coordinate={output} />);
  }

  let current_dataset = config["dataset"]["current_dataset"];
  let topic = config["dataset"][current_dataset];

  let year = config["dataset"]["year"];

  for (let country in dataset[topic]) {
    console.log(country, "in", year, dataset[topic][country][year]);
  }

  return (
    <View style={styles.page}>
      <View style={styles.container}>
        <MapboxGL.MapView style={styles.map}>
          <MapboxGL.Camera 
            animationDuration={0}
            zoomLevel={3}
            centerCoordinate={[7.737262, 51.050407]}
          />
          <MapboxGL.ShapeSource id="countries" shape={countries} onPress={(e) => {console.log("Clicked on", e.features[0].properties.admin);}}>
            <MapboxGL.FillLayer
              id="fillCountries" 
              style={{ fillColor: "yellow", fillOpacity: 0.5 }} 
            />
            <MapboxGL.LineLayer
              id="lineCountries"
              style={{ lineColor: "white", lineWidth: 2 }}
            />
          </MapboxGL.ShapeSource>
          {points}
        </MapboxGL.MapView>
      </View>
    </View>
  );
}

const styles = StyleSheet.create({
  page: {
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
    backgroundColor: '#F5FCFF'
  },
  container: {
    height: '100%',
    width: '100%',
    backgroundColor: 'blue'
  },
  map: {
    flex: 1
  }
});

export default App;
