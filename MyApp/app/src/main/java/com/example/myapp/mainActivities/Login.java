package com.example.myapp.mainActivities;

import androidx.appcompat.app.AppCompatActivity;

import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.LinearLayout;

import com.example.myapp.R;

public class Login extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_login);

        LinearLayout layoutLogin = findViewById(R.id.layoutLogin);
        LinearLayout layoutVerification = findViewById(R.id.layoutVerification);
        LinearLayout layoutChange = findViewById(R.id.layoutChange);
        LinearLayout layoutUsername = findViewById(R.id.layoutUsername);
        LinearLayout layoutPassword = findViewById(R.id.layoutPassword);
        LinearLayout layoutConfirmation = findViewById(R.id.layoutConfirmation);
    }
}