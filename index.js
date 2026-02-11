//Event handler for highlighting link on mouse hover
function highlightLink(element) {
    element.style.backgroundColor = 'pink';
}

//Event handler for unhighlighting link on mouse out
function unhighlightLink(element) {
    element.style.backgroundColor = '';
}

//Event handler to change truck to ambulance on button click
function changeToAmbulance(){
    document.getElementById('myImage').src='visuals/ambulance.png';
}

//Event handler to change ambulance to truck on button click
function changeToTruck(){
    document.getElementById('myImage').src='visuals/firetruck.png';
}