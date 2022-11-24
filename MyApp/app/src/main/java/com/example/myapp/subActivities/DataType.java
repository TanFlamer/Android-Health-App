package com.example.myapp.subActivities;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.DialogFragment;

import com.example.myapp.R;
import com.google.android.material.textfield.TextInputLayout;

public class DataType extends DialogFragment {

    EditText name, calorie;
    TextInputLayout nameInput, calorieInput;
    Button buttonSave, buttonReturn;

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        return inflater.inflate(R.layout.data_type, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        name = view.findViewById(R.id.name);
        calorie = view.findViewById(R.id.calorie);

        nameInput = view.findViewById(R.id.nameInput);
        calorieInput = view.findViewById(R.id.calorieInput);

        buttonSave = view.findViewById(R.id.buttonSave);
        buttonReturn = view.findViewById(R.id.buttonReturn);
        buttonReturn.setOnClickListener(v -> dismiss());
    }

    /*public boolean validateName(TextInputLayout textInputLayout, EditText editText){
        String nameText = editText.getText().toString();
        boolean hasFocus = editText.hasFocus();
        boolean emptyUsername = nameText.isEmpty();
        boolean validUsername = !emptyUsername && accountViewModel.validateUsername(nameText);

        if(!hasFocus || validUsername)
            textInputLayout.setErrorEnabled(false);
        else if(emptyUsername)
            textInputLayout.setError("Username cannot be empty");
        else
            textInputLayout.setError("Username already taken");
        return validUsername;
    }*/
}
