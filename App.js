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
  //console.log("JENKS:", jenks);

  let first_class = [];
  let second_class = [];
  let third_class = [];
  let fourth_class = [];
  let fifth_class = [];

  for(let country of eur_countries){
    let countryValue = dataset[topic][country]["val"];
    let class_number;
    for (let j = 0; j <= no_classes-1; j++)  {
      if (countryValue >= (jenks[j]) && countryValue <= (jenks[j + 1])) {
        class_number = j+1;
      }
    }
    switch (class_number) {
      case 1:
        first_class.push(country);
          break;
      case 2:
        second_class.push(country);
          break;
      case 3:
        third_class.push(country);
          break;
      case 4:
        fourth_class.push(country);
          break;
      case 5:
        fifth_class.push(country);
          break;
    }
  }

  // console.log(first_class);
  // console.log(second_class);
  // console.log(third_class);
  // console.log(fourth_class);
  // console.log(fifth_class);

  // let country_eval = "Hungary";
  // console.log();
  // console.log(topic);
  // console.log(country_eval);

  // // LOWER
  // if(second_class.includes(country_eval)){
  //   console.log("LOWER THAN", country_eval, first_class);
  // }else if(third_class.includes(country_eval)){
  //   console.log("LOWER THAN", country_eval, first_class.concat(second_class).sort());
  // }else if(fourth_class.includes(country_eval)){
  //   console.log("LOWER THAN", country_eval, first_class.concat(second_class).concat(third_class).sort());
  // }else if(fifth_class.includes(country_eval)){
  //   console.log("LOWER THAN", country_eval, first_class.concat(second_class).concat(third_class).concat(fourth_class).sort());
  // }else{
  //   console.log("Not possible");
  // }

  // // SAME
  // if(first_class.includes(country_eval)){
  //   console.log("SAME AS", country_eval, first_class);
  // }else if(second_class.includes(country_eval)){
  //   console.log("SAME AS", country_eval, second_class);
  // }else if(third_class.includes(country_eval)){
  //   console.log("SAME AS", country_eval, third_class);
  // }else if(fourth_class.includes(country_eval)){
  //   console.log("SAME AS", country_eval, fourth_class);
  // }else if(fifth_class.includes(country_eval)){
  //   console.log("SAME AS", country_eval, fifth_class);
  // }else{
  //   console.log("Not possible");
  // }

  // // HIGHER
  // if(first_class.includes(country_eval)){
  //   console.log("HIGHER THAN", country_eval, second_class.concat(third_class).concat(fourth_class).concat(fifth_class).sort());
  // }else if(second_class.includes(country_eval)){
  //   console.log("HIGHER THAN", country_eval, third_class.concat(fourth_class).concat(fifth_class).sort());
  // }else if(third_class.includes(country_eval)){
  //   console.log("HIGHER THAN", country_eval, fourth_class.concat(fifth_class).sort());
  // }else if(fourth_class.includes(country_eval)){
  //   console.log("HIGHER THAN", country_eval, fifth_class);
  // }else{
  //   console.log("Not possible");
  // }

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
  let colors_two = [
    '#4668ab',
    '#3b62a7',
    '#2f5ca3',
    '#2057a0',
    '#08519c'
    ];

  let colors_three = [
    '#5572b1',
    '#496aac',
    '#3c63a7',
    '#295aa2',
    '#08519c'
    ];

  let colors_four = [
    '#617ab6',
    '#5371b0',
    '#4166a9',
    '#2c5ba3',
    '#08519c'
    ];
  
  let colors_five = [
    '#7186bd',
    '#5f79b5',
    '#4c6cad',
    '#335ea5',
    '#08519c'
    ];

  let colors_six = [
    '#7e90c3',
    '#687fb9',
    '#5471b0',
    '#3a62a7',
    '#08519c'
    ];
  let colors_seven = [
    '#8b9ac9',
    '#7186bd',
    '#5974b2',
    '#3c63a8',
    '#08519c'
    ];
  
  let colors_eight = [
    '#9ca8d0',
    '#7f90c3',
    '#627bb6',
    '#4266aa',
    '#08519c'
    ];

  let colors_nine = [
    '#adb6d8',
    '#8a99c8',
    '#6980b9',
    '#4869ac',
    '#08519c'
    ];
    
  let colors_ten = [
    '#bac1de',
    '#92a0cc',
    '#6e83bb',
    '#4a6bad',
    '#08519c'
    ];

  let colors_eleven = [
    '#cad0e6',
    '#9faad2',
    '#768abf',
    '#4f6eaf',
    '#08519c'
    ];
  
  let current_col_palette;
  switch (col_dist) {
    case 2:
      current_col_palette = colors_two;
        break;
    case 3:
      current_col_palette = colors_three;
        break;
    case 4:
      current_col_palette = colors_four;
        break;
    case 5:
      current_col_palette = colors_five;
        break;
    case 6:
      current_col_palette = colors_six;
        break;
    case 7:
      current_col_palette = colors_seven;
        break;
    case 8:
      current_col_palette = colors_eight;
        break;
    case 9:
      current_col_palette = colors_nine;
        break;
    case 10:
      current_col_palette = colors_ten;
        break;
    case 11:
      current_col_palette = colors_eleven;
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
    height: 300,
    width: 150,
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
    height: 35, 
    width: 35
  }, 
  itemText: {
    fontSize: 9,
  }
});

export default App;
