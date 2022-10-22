package com.example.myapp;

import androidx.appcompat.app.AppCompatActivity;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.os.Bundle;

import com.google.android.material.bottomnavigation.BottomNavigationView;

public class Exercise extends AppCompatActivity {

    BottomNavigationView bottomNavigation;

    @SuppressLint("NonConstantResourceId")
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_exercise);

        //Initialization
        BottomNavigationView bottomNavigationView = findViewById(R.id.bottom_navigator);
        bottomNavigation = bottomNavigationView;

        //Select MP3 as default
        bottomNavigationView.setSelectedItemId(R.id.exercise);

        //Item Selected Listener
        bottomNavigationView.setOnItemSelectedListener(item -> {
            switch (item.getItemId()){
                case R.id.save:
                    startActivity(new Intent(getApplicationContext(), Save.class));
                    overridePendingTransition(0, 0);
                    return true;

                case R.id.sleep:
                    startActivity(new Intent(getApplicationContext(), Sleep.class));
                    overridePendingTransition(0, 0);
                    return true;

                case R.id.mp3:
                    startActivity(new Intent(getApplicationContext(), MainActivity.class));
                    overridePendingTransition(0, 0);
                    return true;

                case R.id.exercise:
                    return true;

                case R.id.info:
                    startActivity(new Intent(getApplicationContext(), Info.class));
                    overridePendingTransition(0, 0);
                    return true;
            }
            return false;
        });
    }

    @Override
    protected void onResume() {
        super.onResume();
        bottomNavigation.setSelectedItemId(R.id.exercise);
    }
}