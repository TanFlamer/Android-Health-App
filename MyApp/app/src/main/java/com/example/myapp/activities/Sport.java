package com.example.myapp.activities;

import androidx.appcompat.app.AppCompatActivity;
import androidx.viewpager2.widget.ViewPager2;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.os.Bundle;

import com.example.myapp.R;
import com.example.myapp.fragmentsSleep.SleepCalendar;
import com.example.myapp.fragmentsSleep.SleepFragmentAdapter;
import com.example.myapp.fragmentsSleep.SleepList;
import com.example.myapp.fragmentsSleep.SleepStatistics;
import com.example.myapp.fragmentsSport.SportCalendar;
import com.example.myapp.fragmentsSport.SportFragmentAdapter;
import com.example.myapp.fragmentsSport.SportList;
import com.example.myapp.fragmentsSport.SportStatistics;
import com.example.myapp.fragmentsSport.SportType;
import com.google.android.material.bottomnavigation.BottomNavigationView;
import com.google.android.material.tabs.TabLayout;

public class Sport extends AppCompatActivity {

    BottomNavigationView bottomNavigation;

    @SuppressLint("NonConstantResourceId")
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_sport);

        //Initialization
        BottomNavigationView bottomNavigationView = findViewById(R.id.bottom_navigator);
        bottomNavigation = bottomNavigationView;

        //Select MP3 as default
        bottomNavigationView.setSelectedItemId(R.id.sport);

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

                case R.id.music:
                    startActivity(new Intent(getApplicationContext(), Music.class));
                    overridePendingTransition(0, 0);
                    return true;

                case R.id.sport:
                    return true;

                case R.id.info:
                    startActivity(new Intent(getApplicationContext(), Info.class));
                    overridePendingTransition(0, 0);
                    return true;
            }
            return false;
        });

        ViewPager2 viewPager2 = findViewById(R.id.viewpagerSport);
        SportFragmentAdapter sportFragmentAdapter = new SportFragmentAdapter(getSupportFragmentManager(), getLifecycle());

        sportFragmentAdapter.addFragment(new SportList());
        sportFragmentAdapter.addFragment(new SportCalendar());
        sportFragmentAdapter.addFragment(new SportType());
        sportFragmentAdapter.addFragment(new SportStatistics());
        viewPager2.setAdapter(sportFragmentAdapter);

        TabLayout tabLayout = findViewById(R.id.layoutSport);
        tabLayout.addOnTabSelectedListener(new TabLayout.OnTabSelectedListener() {
            @Override
            public void onTabSelected(TabLayout.Tab tab) {
                viewPager2.setCurrentItem(tab.getPosition());
            }

            @Override
            public void onTabUnselected(TabLayout.Tab tab) {

            }

            @Override
            public void onTabReselected(TabLayout.Tab tab) {

            }
        });

        viewPager2.registerOnPageChangeCallback(new ViewPager2.OnPageChangeCallback() {
            @Override
            public void onPageSelected(int position) {
                tabLayout.selectTab(tabLayout.getTabAt(position));
            }
        });
    }

    @Override
    protected void onResume() {
        super.onResume();
        bottomNavigation.setSelectedItemId(R.id.sport);
    }
}