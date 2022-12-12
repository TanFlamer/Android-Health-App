package com.example.myapp.subActivities.type;

import android.os.Bundle;
import android.text.Editable;
import android.text.InputFilter;
import android.text.TextWatcher;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.Button;
import android.widget.EditText;

import androidx.appcompat.app.AppCompatActivity;
import androidx.lifecycle.ViewModelProvider;

import com.example.myapp.R;
import com.example.myapp.databasefiles.type.Type;
import com.google.android.material.textfield.TextInputLayout;

import java.util.Objects;

public class TypeDataActivity extends AppCompatActivity {

    TypeDataViewModel typeDataViewModel;
    EditText name, calorie;
    TextInputLayout nameInput, calorieInput;
    Button buttonSave, buttonReturn;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.data_type);
        //get view model
        typeDataViewModel = new ViewModelProvider(this).get(TypeDataViewModel.class);
        //load existing sport type if name given
        typeDataViewModel.loadType(getIntent().getStringExtra("typeName"));
        //show back button on top
        Objects.requireNonNull(getSupportActionBar()).setDisplayHomeAsUpEnabled(true);
        //initialise all components
        initialiseAll();
    }

    //initialise all components
    public void initialiseAll(){
        //link all components by ID
        initialiseComponentID();
        //initialise edit text for user input
        initialiseEditTexts();
        //initialise save and return button
        initialiseButtons();
    }

    //link all components by ID
    public void initialiseComponentID(){
        name = findViewById(R.id.name);
        calorie = findViewById(R.id.calorie);
        buttonSave = findViewById(R.id.buttonSave);
        buttonReturn = findViewById(R.id.buttonReturn);
    }

    //initialise edit text for user input
    public void initialiseEditTexts(){
        //get sport type name text view by ID
        nameInput = findViewById(R.id.nameInput);
        //force uppercase
        name.setFilters(new InputFilter[] {new InputFilter.AllCaps()});
        //add text view with text watcher and on focus listener
        name.addTextChangedListener(typeTextWatcher);
        name.setOnFocusChangeListener((v, hasFocus) -> validateName());
        //get sport type calorie text view by ID
        calorieInput = findViewById(R.id.calorieInput);
        //add text view with text watcher and on focus listener
        calorie.addTextChangedListener(typeTextWatcher);
        calorie.setOnFocusChangeListener((v, hasFocus) -> validateDouble());
        //fill sport name and calorie text views with initial strings
        Type type = typeDataViewModel.getType();
        name.setText(type == null ? "" : type.getTypeName());
        calorie.setText(type == null ? "" : String.valueOf(type.getCaloriePerMinute()));
    }

    //initialise save and return button
    public void initialiseButtons(){
        //set save button on click listener
        buttonSave.setOnClickListener(v -> {
            //get old sport type
            Type type = typeDataViewModel.getType();
            //get new sport type name
            String newTypeName = name.getText().toString();
            //get new sport type calorie
            double newTypeDouble = Double.parseDouble(calorie.getText().toString());
            if(type == null) //if old sport type is null
                typeDataViewModel.insert(newTypeName, newTypeDouble); //insert new sport type
            else
                typeDataViewModel.update(newTypeName, newTypeDouble); //update old sport type if name or calorie different
            finish(); //return to last activity
        });
        //set return button on click listener to return to last activity
        buttonReturn.setOnClickListener(v -> finish());
    }

    //validate sport type name
    public boolean validateName(){
        //get sport type name entered
        String typeNameText = name.getText().toString();
        //get old sport type
        Type type = typeDataViewModel.getType();
        //check if text view has focus
        boolean hasFocus = name.hasFocus();
        //check if sport type name empty
        boolean emptyTypeName = typeNameText.isEmpty();
        //check if sport type name same as old name
        boolean oldTypeName = type == null || typeNameText.equals(type.getTypeName());
        //check if sport type name taken
        boolean validTypeName = !emptyTypeName && (oldTypeName || typeDataViewModel.validateTypeName(typeNameText));

        if(!hasFocus || validTypeName) //if no focus or valid name
            nameInput.setErrorEnabled(false); //show no error
        else if(emptyTypeName) //if empty name
            nameInput.setError("Type name cannot be empty"); //show error
        else //if invalid name
            nameInput.setError("Type name already taken"); //show error
        return validTypeName; //return validity of sport type name
    }

    //validate sport type calorie
    public boolean validateDouble(){
        //get sport type calorie entered
        String typeCalorieText = calorie.getText().toString();
        //check if text view has focus
        boolean hasFocus = calorie.hasFocus();
        //check if sport type calorie empty
        boolean emptyTypeCalorie = typeCalorieText.isEmpty();
        //check if sport calorie valid double
        boolean validTypeCalorie = !emptyTypeCalorie && isDouble(typeCalorieText);
        //check if sport calorie positive value
        boolean positiveTypeCalorie = validTypeCalorie && Double.parseDouble(typeCalorieText) > 0;

        if(!hasFocus || positiveTypeCalorie) //if no focus or valid double
            calorieInput.setErrorEnabled(false); //show no error
        else if(emptyTypeCalorie) //if empty calorie
            calorieInput.setError("Calories per minute cannot be empty"); //show error
        else if(!validTypeCalorie) //if invalid calorie
            calorieInput.setError("Calories per minute must be double"); //show error
        else //if zero or negative calorie
            calorieInput.setError("Calories per minute cannot be zero or negative"); //show error
        return positiveTypeCalorie; //return validity of sport type calorie
    }

    //check if there is difference with old sport type
    public boolean differentSportType(){
        //get old sport type
        Type type = typeDataViewModel.getType();
        //get sport type name entered
        String typeNameText = name.getText().toString();
        //get sport type calorie entered
        String typeCalorieText = calorie.getText().toString();
        //check if there is difference with old sport type
        return type == null || !type.getTypeName().equals(typeNameText) || !type.getCaloriePerMinute().toString().equals(typeCalorieText);
    }

    //check if string is valid double
    public boolean isDouble(String calorieText){
        try{
            //if valid double, return true
            Double.parseDouble(calorieText);
            return true;
        }
        catch (NumberFormatException e){
            //else return false
            return false;
        }
    }

    //text watcher for sport type name and calorie
    private final TextWatcher typeTextWatcher = new TextWatcher() {
        @Override
        public void beforeTextChanged(CharSequence s, int start, int count, int after) {

        }

        @Override
        public void onTextChanged(CharSequence s, int start, int before, int count) {
            //check if valid sport type name
            boolean validTypeName = validateName();
            //check if valid sport type calorie
            boolean validTypeCalorie = validateDouble();
            //check if there is difference with old sport type
            boolean differentSportType = differentSportType();
            //enable save button if valid sport typename and calorie
            buttonSave.setEnabled(validTypeName && validTypeCalorie && differentSportType);
        }

        @Override
        public void afterTextChanged(Editable s) {

        }
    };

    @Override //set back button on top to return to last activity
    public boolean onOptionsItemSelected(MenuItem item) {
        if (item.getItemId() == android.R.id.home) {
            finish();
            return true;
        }

        return super.onOptionsItemSelected(item);
    }

    //initialise options menu
    public boolean onCreateOptionsMenu(Menu menu) {
        return true;
    }
}
