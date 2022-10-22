package com.example.myappv2;

import androidx.appcompat.app.AppCompatActivity;
import androidx.fragment.app.FragmentManager;

import android.annotation.SuppressLint;
import android.os.Bundle;
import android.view.MenuItem;

import com.example.myappv2.mainfragments.Info;
import com.example.myappv2.mainfragments.Music;
import com.example.myappv2.mainfragments.Save;
import com.example.myappv2.mainfragments.Sleep;
import com.example.myappv2.mainfragments.Sports;
import com.google.android.material.bottomnavigation.BottomNavigationView;

import java.util.Stack;

public class MainActivity extends AppCompatActivity {

    BottomNavigationView bottomNavigation;
    Stack<Integer> menuPresses = new Stack<>();

    @SuppressLint("NonConstantResourceId")
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        //Initialization
        BottomNavigationView bottomNavigationView = findViewById(R.id.bottom_navigator);
        bottomNavigation = bottomNavigationView;

        //Select MP3 as default
        bottomNavigationView.setSelectedItemId(R.id.music);
        menuPresses.add(R.id.music);

        //Manage fragments
        FragmentManager fragmentManager = getSupportFragmentManager();

        //Item Selected Listener
        bottomNavigationView.setOnItemSelectedListener(item -> {
            fragmentManager.beginTransaction()
                    .replace(R.id.mainFragmentContainer, returnFragmentClass(item), null)
                    .commit();
            return true;
        });
    }

    public Class returnFragmentClass(MenuItem item){
        int id = item.getItemId();
        menuPresses.add(id);
        if(id == R.id.save)
            return Save.class;
        else if(id == R.id.sleep)
            return Sleep.class;
        else if(id == R.id.music)
            return Music.class;
        else if(id == R.id.exercise)
            return Sports.class;
        else if(id == R.id.info)
            return Info.class;
        return null;
    }

    @Override
    public void onBackPressed() {
        menuPresses.pop();
        if(menuPresses.empty())
            super.onBackPressed();
        else
            bottomNavigation.setSelectedItemId(menuPresses.pop());
    }
}