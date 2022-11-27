package com.example.myapp.mainActivities.music;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.widget.ImageButton;
import android.widget.SeekBar;
import android.widget.TextView;

import androidx.appcompat.app.AppCompatActivity;
import androidx.lifecycle.Observer;
import androidx.viewpager2.widget.ViewPager2;

import com.example.myapp.MainApplication;
import com.example.myapp.MusicPlayer;
import com.example.myapp.R;
import com.example.myapp.databaseFiles.song.Song;
import com.example.myapp.mainActivities.info.Info;
import com.example.myapp.mainActivities.save.Save;
import com.example.myapp.fragments.music.MusicFragmentAdapter;
import com.example.myapp.fragments.music.musicPlaylists.MusicPlaylists;
import com.example.myapp.fragments.music.musicList.MusicList;
import com.example.myapp.fragments.music.musicStatistics.MusicStatistics;
import com.example.myapp.mainActivities.sleep.Sleep;
import com.example.myapp.mainActivities.sport.Sport;
import com.google.android.material.bottomnavigation.BottomNavigationView;
import com.google.android.material.tabs.TabLayout;

public class Music extends AppCompatActivity {

    BottomNavigationView bottomNavigation;
    ViewPager2 viewPager2;
    TabLayout tabLayout;

    MusicPlayer musicPlayer;
    TextView songName;
    SeekBar songProgress;
    ImageButton songPrevious, songPause, songNext;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_music);
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
    }

    public void initialiseViewPager(){
        viewPager2 = findViewById(R.id.viewpagerMusic);
        MusicFragmentAdapter musicFragmentAdapter = new MusicFragmentAdapter(getSupportFragmentManager(), getLifecycle());

        musicFragmentAdapter.addFragment(new MusicList());
        musicFragmentAdapter.addFragment(new MusicPlaylists());
        musicFragmentAdapter.addFragment(new MusicStatistics());
        viewPager2.setAdapter(musicFragmentAdapter);

        tabLayout = findViewById(R.id.layoutMusic);
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
        bottomNavigation.setSelectedItemId(R.id.music);
    }
}