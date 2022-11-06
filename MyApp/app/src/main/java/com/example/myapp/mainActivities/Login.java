package com.example.myapp.mainActivities;

import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;

import android.content.DialogInterface;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.LinearLayout;

import com.example.myapp.R;
import com.example.myapp.subActivities.DataMusic;
import com.example.myapp.subActivities.DataSleep;

public class Login extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_login);

        Button buttonGuest = findViewById(R.id.buttonGuest);
        buttonGuest.setOnClickListener(view -> new AlertDialog.Builder(this)
                .setTitle("Guest Login")
                .setMessage("Are you sure you want to login as guest? Any changes made will not be saved.")
                .setPositiveButton("Yes", (dialogInterface, i) -> {
                    startActivity(new Intent(getApplicationContext(), Music.class));
                    overridePendingTransition(0, 0);
                })
                .setNegativeButton("No", null)
                .create()
                .show());

        Button buttonLogin = findViewById(R.id.buttonLogin);
        buttonLogin.setOnClickListener(view -> {
            startActivity(new Intent(getApplicationContext(), Account.class));
            overridePendingTransition(0, 0);
        });
    }
}