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
  
  const vis_type = config["visualization_type"];

  const current_dataset = config["dataset"]["current_dataset"];
  const topic = config["dataset"][current_dataset];

  const year = config["dataset"]["year"];

  let eur_countries = [];
  for (let country in dataset[topic]) {
    eur_countries.push(country);
  }

  let graduated_symbols = [];

  // calculate center of polygon
  for(let i = 0, l = countries.features.length; i < l; i++){
    let country = countries.features[i].properties.admin;
    
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

    let size = "";

    if(eur_countries.includes(country)){
      let incidence = dataset[topic][country][year];
      if(incidence > 25){
        size = 40;
      }else{
        size = 20;
      }

      graduated_symbols.push(
        <MapboxGL.PointAnnotation
          key={i}
          id={country}
          coordinate={output}
        >
          <View
            style={{
              height: size,
              width: size,
              backgroundColor: "green",
              borderRadius: 50,
              borderColor: "#fff",
              borderWidth: 2,
              opacity: 0.5
            }}
          />
        </MapboxGL.PointAnnotation>
      );
    }
  }


  let choropleth_countries = [];

  for(let i = 0, l = countries.features.length; i < l; i++){
    let country = countries.features[i].properties.admin;
    let color = "";

    if(eur_countries.includes(country)){
      let incidence = dataset[topic][country][year];
      if(incidence > 25){
        color = "red";
      }else{
        color = "green";
      }
    }else{
      color = "grey";
    }

    choropleth_countries.push(
      <MapboxGL.ShapeSource key = {i} id={country} shape={countries.features[i]} onPress={(e) => {console.log("Clicked on", e.features[0].properties.admin);}}>
          <MapboxGL.FillLayer
            id={country+"fill"} 
            style={{ fillColor: color, fillOpacity: 0.5 }} 
          />
          <MapboxGL.LineLayer
            id={country+"border"}
            style={{ lineColor: "white", lineWidth: 2 }}
          />
        </MapboxGL.ShapeSource>
    );
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
          {vis_type == "choropleth" ? choropleth_countries : graduated_symbols}
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
