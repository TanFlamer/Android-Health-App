package com.example.myapp.mainActivities.sleep;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.os.Bundle;
import android.widget.ImageButton;
import android.widget.SeekBar;
import android.widget.TextView;

import androidx.appcompat.app.AppCompatActivity;
import androidx.viewpager2.widget.ViewPager2;

import com.example.myapp.MainApplication;
import com.example.myapp.MusicPlayer;
import com.example.myapp.R;
import com.example.myapp.fragments.sleep.SleepFragmentAdapter;
import com.example.myapp.fragments.sleep.sleepCalendar.SleepCalendar;
import com.example.myapp.fragments.sleep.sleepChart.SleepChart;
import com.example.myapp.fragments.sleep.sleepList.SleepList;
import com.example.myapp.fragments.sleep.sleepStatistics.SleepStatistics;
import com.example.myapp.mainActivities.info.Info;
import com.example.myapp.mainActivities.music.Music;
import com.example.myapp.mainActivities.save.Save;
import com.example.myapp.mainActivities.sport.Sport;
import com.google.android.material.bottomnavigation.BottomNavigationView;
import com.google.android.material.tabs.TabLayout;

public class Sleep extends AppCompatActivity {

    BottomNavigationView bottomNavigation;
    ViewPager2 viewPager2;
    TabLayout tabLayout;

    MusicPlayer musicPlayer;
    TextView songName;
    SeekBar songProgress;
    ImageButton songPrevious, songPause, songNext;

    @SuppressLint("NonConstantResourceId")
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_sleep);
        initialiseBottomNavigator();
        initialiseViewPager();
        initialiseMusicPlayer();
    }

    @SuppressLint("NonConstantResourceId")
    public void initialiseBottomNavigator(){
        bottomNavigation = findViewById(R.id.bottom_navigator);
        bottomNavigation.setSelectedItemId(R.id.music);
        bottomNavigation.setOnItemSelectedListener(item -> {
            switch (item.getItemId()){
                case R.id.save:
                    startActivity(new Intent(getApplicationContext(), Save.class));
                    overridePendingTransition(0, 0);
                    return true;

                case R.id.sleep:
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
    }

    public void initialiseViewPager(){
        viewPager2 = findViewById(R.id.viewpagerSleep);
        SleepFragmentAdapter sleepFragmentAdapter = new SleepFragmentAdapter(getSupportFragmentManager(), getLifecycle());

        sleepFragmentAdapter.addFragment(new SleepList());
        sleepFragmentAdapter.addFragment(new SleepChart());
        sleepFragmentAdapter.addFragment(new SleepCalendar());
        sleepFragmentAdapter.addFragment(new SleepStatistics());
        viewPager2.setAdapter(sleepFragmentAdapter);

        tabLayout = findViewById(R.id.layoutSleep);
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

    public void initialiseMusicPlayer(){
        musicPlayer = ((MainApplication) getApplication()).getMusicPlayer();
        initialiseSongController();
        initialiseImageButtons();
        initialiseLiveData();
    }

    public void initialiseSongController(){
        songName = findViewById(R.id.songName);
        songProgress = findViewById(R.id.songProgress);
        songProgress.setOnSeekBarChangeListener(new SeekBar.OnSeekBarChangeListener() {
            @Override
            public void onProgressChanged(SeekBar seekBar, int progress, boolean fromUser) {
                if(fromUser) musicPlayer.setSongProgress(progress);
            }

            @Override
            public void onStartTrackingTouch(SeekBar seekBar) {
                musicPlayer.pauseSong();
            }

            @Override
            public void onStopTrackingTouch(SeekBar seekBar) {
                musicPlayer.playSong();
            }
        });
    }

    public void initialiseImageButtons(){
        songPrevious = findViewById(R.id.songPrevious);
        songPrevious.setOnClickListener(v -> musicPlayer.previousButton());
        songPause = findViewById(R.id.songPause);
        songPause.setOnClickListener(v -> musicPlayer.playButton());
        songNext = findViewById(R.id.songNext);
        songNext.setOnClickListener(v -> musicPlayer.nextButton());
    }

    public void initialiseLiveData(){
        musicPlayer.getSong().observeForever(song -> {
            songName.setText(song.getSongName());
            songProgress.setProgress(0);
            songProgress.setMax(song.getSongDuration() * 1000);
        });
        musicPlayer.getSongProgress().observeForever(integer -> songProgress.setProgress(integer));
    }

    @Override
    protected void onResume() {
        super.onResume();
        bottomNavigation.setSelectedItemId(R.id.sleep);
    }
}