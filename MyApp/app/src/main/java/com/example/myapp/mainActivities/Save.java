package com.example.myapp.mainActivities;

import androidx.appcompat.app.AppCompatActivity;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.os.Bundle;
import android.widget.ListView;

import com.example.myapp.R;
import com.example.myapp.mainActivities.listSave.SaveListAdapter;
import com.example.myapp.mainActivities.listSave.SaveListItem;
import com.google.android.material.bottomnavigation.BottomNavigationView;

import java.util.ArrayList;
import java.util.List;

public class Save extends AppCompatActivity {

    BottomNavigationView bottomNavigation;

    @SuppressLint("NonConstantResourceId")
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_save);

        //Initialization
        BottomNavigationView bottomNavigationView = findViewById(R.id.bottom_navigator);
        bottomNavigation = bottomNavigationView;

        //Select MP3 as default
        bottomNavigationView.setSelectedItemId(R.id.save);

        //Item Selected Listener
        bottomNavigationView.setOnItemSelectedListener(item -> {
            switch (item.getItemId()){
                case R.id.save:
                    return true;

                case R.id.sleep:
                    startActivity(new Intent(getApplicationContext(), Sleep.class));
                    overridePendingTransition(0, 0);
                    return true;

                case R.id.music:
                    startActivity(new Intent(getApplicationContext(), Music.class));
                    overridePendingTransition(0, 0);
                    return true;

                case R.id.sport:
                    startActivity(new Intent(getApplicationContext(), Sport.class));
                    overridePendingTransition(0, 0);
                    return true;

                case R.id.info:
                    startActivity(new Intent(getApplicationContext(), Info.class));
                    overridePendingTransition(0, 0);
                    return true;
            }
            return false;
        });

        ListView listView = findViewById(R.id.saveListView);
        List<SaveListItem> saveListItemList = new ArrayList<>();

        saveListItemList.add(new SaveListItem("test", 0));
        saveListItemList.add(new SaveListItem("test1", 1));
        saveListItemList.add(new SaveListItem("test2", 0));
        saveListItemList.add(new SaveListItem("test3", 1));
        saveListItemList.add(new SaveListItem("test4", 0));
        saveListItemList.add(new SaveListItem("test5", 1));

        SaveListAdapter saveListAdapter = new SaveListAdapter(this, R.layout.save_list_item, saveListItemList);
        listView.setAdapter(saveListAdapter);
    }

    @Override
    protected void onResume() {
        super.onResume();
        bottomNavigation.setSelectedItemId(R.id.save);
    }
}