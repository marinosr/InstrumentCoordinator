int currentseven = 1;

void setup() {
    Serial.begin(9600);
    pinMode(4, OUTPUT);
    pinMode(5, OUTPUT);
    pinMode(6, OUTPUT);
    pinMode(7, OUTPUT);
    digitalWrite(4,1);
    digitalWrite(5,1);
    digitalWrite(6,1);
    digitalWrite(7,1);
}

void loop() {
 char buffer[8];
 while(!Serial.available()); // Wait until there is serial data received. 
 int size = Serial.readBytesUntil('\n', buffer, 8); // Read up to 8 bytes of serial data, until '\n' is received
 Serial.write(buffer, size);

int state = 0;
 
 switch(buffer[0]){
  case 'V':  //If requesting a valve switch
  state = buffer[3] - '0'; // Determine whether it should be off or on. (The -'0' converts from character to integer)
  switch(buffer[1]){ // This reads the second character in the buffer to determine which valve wants to be switched.
    case '1':
    digitalWrite(4, state); //Sets the pin to the desired state. 
    Serial.write(" OK\n");
    break;
    case '2':
    digitalWrite(5, state);
    Serial.write(" OK\n");
    break;
    case '3':
    digitalWrite(6, state);
    Serial.write(" OK\n");
    break;
    case '4':
    Serial.write(state);
   if(state==35){ // If the 4th character of read command is 'S', ('S'-'0'=35) then read what the current state of the pin is, and set the desired state to the opposite.'S' is 'switch'
      if(currentseven==1){
        state = 0;
      } else {
        state = 1;
      }
    }
    digitalWrite(7, state);
    currentseven = state; 
    Serial.write(" OK\n");
    break;
  }
  break;
  case 'R':
  Serial.write(" Liaison ready.\n");
  break;
      
    }
  }
 
