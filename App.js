import React from 'react';
import { StyleSheet, View, Text } from 'react-native';
import MapboxGL from '@react-native-mapbox-gl/maps';
import polylabel from "@mapbox/polylabel";
import * as d3 from "d3";
import { countries } from "./data/countries_LOW";
import * as dataset from './data/dataset.json';
import * as config from './data/config.json';
import geostats from 'geostats';


MapboxGL.setAccessToken('pk.eyJ1Ijoic3VsYXhhbjI3IiwiYSI6ImNpc3JqNXRidTAwNHAyeXBiY2hxdnlsMG4ifQ.IGQkafPlHEjq7_hno0AnrA');

const App = () => {
  
  //console.log(countries.features.length +  " countries are part of the feature collection.");
  
  const vis_type = config["visualization_type"];

  const current_dataset = config["dataset"]["current_dataset"];
  const topic = config["dataset"][current_dataset];

  const current_col_dist = config["choropleth"]["color"]["current_color_distance"];
  const col_dist = config["choropleth"]["color"][current_col_dist];
  
  const current_constant = config["graduated_symbol"]["constant"]["current_constant"];
  const constant = config["graduated_symbol"]["constant"][current_constant];

  const no_classes = 5;

  // jenks calculation
  let eur_countries = [];
  let dataValues = [];

  for (let country in dataset[topic]) {
    eur_countries.push(country);
    let countryValue = dataset[topic][country]["val"];
    if (countryValue !== "NA") {		
      dataValues.push(countryValue);  
    }
  }
  
  let geoSeries = new geostats(dataValues);
  let jenks = geoSeries.getClassJenks(no_classes);
  console.log("JENKS:", jenks);

  // calculating average size distance in dataset
  // let avg_size_dist = ((jenks[2]/jenks[1])+(jenks[3]/jenks[2])+(jenks[4]/jenks[3])+(jenks[5]/jenks[4]))/(no_classes-1);
  // console.log("AVERAGE SIZE DISTANCE: " + current_dataset , avg_size_dist);

  // GRADUATED SYMBOL MAP
  // let act_radius = [];
  // let radius = [];
  // for (let i = 0; i < jenks.length-1; i++) {
  //   let r = (Math.sqrt(jenks[i]/Math.PI));
  //   act_radius.push(r);
  //   let d = r * 2 * constant;
  //   radius.push(d);
  // }

  // let sizes = radius;

  // console.log("actual radius: " + act_radius);
  // console.log("diameter multiplied by constant: " + radius);
  
  let sizes = [10];
  for(let i= 1; i<=4; i++){
    sizes.push(sizes[0] + i * constant);
  }
  console.log(sizes);

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
      let incidence = dataset[topic][country]["val"];
      if(incidence !== "NA"){
        for (let j = 0; j <= no_classes-1; j++)  {
          if (incidence >= (jenks[j]) && incidence <= (jenks[j + 1])) {
            size = sizes[j];
          }
        }

        graduated_symbols.push(
          <MapboxGL.PointAnnotation
            key={i}
            id={country}
            coordinate={output}
            onSelected={(e) => {console.log("There was a", topic.toLowerCase().slice(0, -4), "of", incidence, "% in", e.properties.id+"!");}}
          >
            <View
              style={{
                height: size,
                width: size,
                backgroundColor: "green",
                borderRadius: size/2,
                borderColor: "black",
                borderWidth: 1,
              }}
            />
          </MapboxGL.PointAnnotation>
        );
      }
    }
  }

  //CHOROPLETH MAPS
  let colors_four = [
    '#617ab6',
    '#5371b0',
    '#4166a9',
    '#2c5ba3',
    '#08519c'
    ];
  
  let colors_six = [
    '#7e90c3',
    '#687fb9',
    '#5471b0',
    '#3a62a7',
    '#08519c'
    ];

  let colors_eight = [
    '#9ca8d0',
    '#7f90c3',
    '#627bb6',
    '#4266aa',
    '#08519c'
    ];
    
  let colors_ten = [
    '#bac1de',
    '#92a0cc',
    '#6e83bb',
    '#4a6bad',
    '#08519c'
    ];

  let colors_twelve = [
    '#d9dded',
    '#a9b3d7',
    '#7d8fc2',
    '#5270b0',
    '#08519c'
    ];
  
  let current_col_palette;
  switch (col_dist) {
    case 4:
      current_col_palette = colors_four;
        break;
    case 6:
      current_col_palette = colors_six;
        break;
    case 8:
      current_col_palette = colors_eight;
        break;
    case 10:
      current_col_palette = colors_ten;
        break;
    case 12:
      current_col_palette = colors_twelve;
        break;
  }

  let choropleth_countries = [];

  for(let i = 0, l = countries.features.length; i < l; i++){
    let country = countries.features[i].properties.admin;
    let color = "";
    let incidence = "";
    
    if(eur_countries.includes(country)){
      incidence = dataset[topic][country]["val"];
      if(incidence == "NA"){
        color = "#A8A8A8";
      }
      else{
        for (let j = 0; j <= no_classes-1; j++)  {
          if (incidence >= (jenks[j]) && incidence <= (jenks[j + 1])) {
            color = current_col_palette[j];
          }
        }
      }
    }else{
      color = "#A8A8A8";
    }

    choropleth_countries.push(
      <MapboxGL.ShapeSource 
        key = {i} 
        id={country} 
        shape={countries.features[i]} 
        onPress={(e) => {console.log("There was a", topic.toLowerCase().slice(0, -4), "of", incidence, "% in", e.features[0].properties.admin+"!");}}
      >
        <MapboxGL.FillLayer
          id={country+"fill"} 
          style={{ fillColor: color, fillOpacity: 0.9 }} 
        />
        <MapboxGL.LineLayer
          id={country+"border"}
          style={{ lineColor: "white", lineWidth: 0.5 }}
        />
      </MapboxGL.ShapeSource>
    );
  }

  let labels = [];

  if(vis_type === "choropleth"){
    for (let i = 0; i < jenks.length-1; i++) {
      let from = jenks[i];
      let to = jenks[i + 1];
  
      labels.push(
        <View key={i} style={styles.legendItems}>
          <View style={[styles.itemSymbol, {backgroundColor: current_col_palette[i]}]}></View>
          <Text style={styles.itemText}>{from + " - " + to}</Text>
        </View>
      );
    }

    labels.push(
      <View key={jenks.length-1} style={styles.legendItems}>
        <View style={[styles.itemSymbol, {backgroundColor: '#A8A8A8'}]}></View>
        <Text style={styles.itemText}>NO DATA</Text>
      </View>
    );

  }else{
    for (let i = 0; i < jenks.length-1; i++) {
      let from = jenks[i];
      let to = jenks[i + 1];
  
      labels.push(
        <View key={i} style={styles.legendItems}>
          <View
              style={{
                height: sizes[i],
                width: sizes[i],
                backgroundColor: "green",
                borderRadius: 50,
                borderColor: "black",
                borderWidth: 1,
              }}
            />
          <Text style={styles.itemText}>{from + " - " + to}</Text>
        </View>
      );
    }
  }

  return (
    <View style={styles.page}>
      <View style={styles.container}>
        <MapboxGL.MapView style={styles.map}
          styleURL={MapboxGL.StyleURL.Light}
        >
          <MapboxGL.Camera 
            animationDuration={0}
            zoomLevel={3}
            centerCoordinate={[7.737262, 51.050407]}
          />
          {vis_type == "choropleth" ? choropleth_countries : graduated_symbols}
        </MapboxGL.MapView>
        <View style={styles.legendContainer}>
          <Text style={styles.legendTitle}>{topic}:</Text>
          {labels}
        </View>
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
  },
  legendContainer: {
    flex: 1,
    flexDirection: 'column',
    backgroundColor: "#cfcfcf",
    opacity: 0.9,
    borderRadius: 10,
    borderColor: 'white',
    borderWidth: 1,
    bottom: 30,
    left: 30,
    height: 400,
    width: 175,
    padding: 10,
    position: 'absolute',
    justifyContent: 'space-evenly'
  }, 
  legendTitle: {
    fontSize: 12, 
    fontWeight: "bold"
  },
  legendItems: {
    flexDirection: 'row',
    alignItems: 'center',
    justifyContent: 'space-between'
  },
  itemSymbol: {
    height: 40, 
    width: 40
  }, 
  itemText: {
    fontSize: 9,
  }
});

export default App;
