package com.example.myapp.subActivities.type;

import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.Button;
import android.widget.EditText;

import androidx.appcompat.app.AppCompatActivity;
import androidx.lifecycle.ViewModelProvider;

import com.example.myapp.R;
import com.google.android.material.textfield.TextInputLayout;

import java.util.Objects;

public class DataType extends AppCompatActivity {

    DataTypeViewModel dataTypeViewModel;
    EditText name, calorie;
    TextInputLayout nameInput, calorieInput;
    Button buttonSave, buttonReturn;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.data_type);
        dataTypeViewModel = new ViewModelProvider(this).get(DataTypeViewModel.class);
        Objects.requireNonNull(getSupportActionBar()).setDisplayHomeAsUpEnabled(true);
        initialiseAll();
    }

    public void initialiseAll(){
        initialiseEditTexts();
        initialiseButtons();
    }

    public void initialiseEditTexts(){
        name = findViewById(R.id.name);
        nameInput = findViewById(R.id.nameInput);
        name.addTextChangedListener(typeTextWatcher);
        name.setOnFocusChangeListener((v, hasFocus) -> validateName());

        calorie = findViewById(R.id.calorie);
        calorieInput = findViewById(R.id.calorieInput);
        calorie.addTextChangedListener(typeTextWatcher);
        calorie.setOnFocusChangeListener((v, hasFocus) -> validateDouble());
    }

    public void initialiseButtons(){
        buttonSave = findViewById(R.id.buttonSave);
        //buttonSave.setOnClickListener();
        buttonReturn = findViewById(R.id.buttonReturn);
        buttonReturn.setOnClickListener(v -> finish());
    }

    public boolean validateName(){
        String typeNameText = name.getText().toString();
        String oldTypeName = dataTypeViewModel.getTypeName();

        boolean hasFocus = name.hasFocus();
        boolean emptyTypeName = typeNameText.isEmpty();
        boolean validTypeName = !emptyTypeName && (typeNameText.equals(oldTypeName) || dataTypeViewModel.validateTypeName(typeNameText));

        if(!hasFocus || validTypeName)
            nameInput.setErrorEnabled(false);
        else if(emptyTypeName)
            nameInput.setError("Type name cannot be empty");
        else
            nameInput.setError("Type name already taken");
        return validTypeName;
    }

    public boolean validateDouble(){
        String typeCalorieText = calorie.getText().toString();
        boolean hasFocus = calorie.hasFocus();
        boolean emptyTypeCalorie = typeCalorieText.isEmpty();
        boolean validTypeCalorie = !emptyTypeCalorie && isDouble(typeCalorieText);
        boolean positiveTypeCalorie = validTypeCalorie && Double.parseDouble(typeCalorieText) > 0;

        if(!hasFocus || positiveTypeCalorie)
            calorieInput.setErrorEnabled(false);
        else if(emptyTypeCalorie)
            calorieInput.setError("Calories per minute cannot be empty");
        else if(!validTypeCalorie)
            calorieInput.setError("Calories per minute must be double");
        else
            calorieInput.setError("Calories per minute cannot be zero or negative");
        return positiveTypeCalorie;
    }

    public boolean isDouble(String calorieText){
        try{
            Double.parseDouble(calorieText);
            return true;
        }
        catch (NumberFormatException e){
            return false;
        }
    }

    private final TextWatcher typeTextWatcher = new TextWatcher() {
        @Override
        public void beforeTextChanged(CharSequence s, int start, int count, int after) {

        }

        @Override
        public void onTextChanged(CharSequence s, int start, int before, int count) {
            boolean validTypeName = validateName();
            boolean validTypeCalorie = validateDouble();
            buttonSave.setEnabled(validTypeName && validTypeCalorie);
        }

        @Override
        public void afterTextChanged(Editable s) {

        }
    };

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        if (item.getItemId() == android.R.id.home) {
            finish();
            return true;
        }

        return super.onOptionsItemSelected(item);
    }

    public boolean onCreateOptionsMenu(Menu menu) {
        return true;
    }
}
