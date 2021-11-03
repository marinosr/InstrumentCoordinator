void setup() {
    Serial.begin(9600);
}

void loop() {
 char buffer[8];
 while(!Serial.available()); // Wait until there is serial data received. 
 int size = Serial.readBytesUntil('\n', buffer, 8); // Read up to 8 bytes of serial data, until '\n' is received
 
 switch(buffer[1]){
  case 'V':  //If requesting a valve switch
  int state = buffer[4] - '0'; // Determine whether it should be off or on. (The -'0' converts from character to integer)
  switch(buffer[2]){ // This reads the second character in the buffer to determine which valve wants to be switched.
    case '1':
    digitalWrite(4, state); //Sets the pin to the desired state. 
    serial.write('OK');
    break;
    case '2':
    digitalWrite(5, state);
    serial.write('OK');
    break;
    case '3':
    digitalWrite(6, state);
    serial.write('OK');
    break;
    case '4':
    if(state=35){ // If the 4th character of read command is 'S', ('S'-'0'=35) then read what the current state of the pin is, and set the desired state to the opposite.'S' is 'switch'
      int current = digitalRead(7);
      state = !(current);
    }
    digitalWrite(7, state);
    serial.write('OK');
    break;
  }
  break;
  case 'R':
  serial.write('Liaison ready.');
  break;
      
    }
  }
 