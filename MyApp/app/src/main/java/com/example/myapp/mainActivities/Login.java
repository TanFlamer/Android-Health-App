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

        Button buttonGuest = findViewById(R.id.buttonGuest);
        buttonGuest.setOnClickListener(view -> {
            startActivity(new Intent(getApplicationContext(), Music.class));
            overridePendingTransition(0, 0);
        });

        Button buttonLogin = findViewById(R.id.buttonLogin);
        buttonLogin.setOnClickListener(view -> {
            startActivity(new Intent(getApplicationContext(), Account.class));
            overridePendingTransition(0, 0);
        });
    }
}