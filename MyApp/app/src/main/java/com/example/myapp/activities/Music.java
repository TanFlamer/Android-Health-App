package com.example.myapp.activities;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.os.Bundle;

import androidx.appcompat.app.AppCompatActivity;
import androidx.viewpager2.widget.ViewPager2;

import com.example.myapp.R;
import com.example.myapp.fragmentsMusic.MusicFragmentAdapter;
import com.example.myapp.fragmentsMusic.MusicPlaylists;
import com.example.myapp.fragmentsMusic.MusicList;
import com.example.myapp.fragmentsMusic.MusicStatistics;
import com.google.android.material.bottomnavigation.BottomNavigationView;
import com.google.android.material.tabs.TabLayout;

public class Music extends AppCompatActivity {

    BottomNavigationView bottomNavigation;

    @SuppressLint("NonConstantResourceId")
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_music);

        //Initialization
        BottomNavigationView bottomNavigationView = findViewById(R.id.bottom_navigator);
        bottomNavigation = bottomNavigationView;

        //Select MP3 as default
        bottomNavigationView.setSelectedItemId(R.id.music);

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

        ViewPager2 viewPager2 = findViewById(R.id.viewpagerMusic);
        MusicFragmentAdapter musicFragmentAdapter = new MusicFragmentAdapter(getSupportFragmentManager(), getLifecycle());

        musicFragmentAdapter.addFragment(new MusicList());
        musicFragmentAdapter.addFragment(new MusicPlaylists());
        musicFragmentAdapter.addFragment(new MusicStatistics());
        viewPager2.setAdapter(musicFragmentAdapter);

        TabLayout tabLayout = findViewById(R.id.layoutMusic);
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
        bottomNavigation.setSelectedItemId(R.id.music);
    }
}