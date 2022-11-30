package com.example.myapp.mainActivities;

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
import com.example.myapp.fragments.sport.SportFragmentAdapter;
import com.example.myapp.fragments.sport.sportCalendar.SportCalendarFragment;
import com.example.myapp.fragments.sport.sportChart.SportChartFragment;
import com.example.myapp.fragments.sport.sportList.SportListFragment;
import com.example.myapp.fragments.sport.sportStatistics.SportStatisticsFragment;
import com.example.myapp.fragments.sport.sportType.SportTypeFragment;
import com.example.myapp.mainActivities.save.SaveActivity;
import com.google.android.material.bottomnavigation.BottomNavigationView;
import com.google.android.material.tabs.TabLayout;

public class SportActivity extends AppCompatActivity {

    SportFragmentAdapter sportFragmentAdapter;
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
        setContentView(R.layout.activity_sport);
        initialiseAll();
    }

    public void initialiseAll(){
        initialiseViewByID();
        initialiseBottomNavigator();
        initialiseLayout();
        initialiseMusicPlayer();
    }

    public void initialiseViewByID(){
        musicPlayer = ((MainApplication) getApplication()).getMusicPlayer();
        bottomNavigation = findViewById(R.id.bottom_navigator);
        tabLayout = findViewById(R.id.layoutSport);
        viewPager2 = findViewById(R.id.viewpagerSport);
        songName = findViewById(R.id.songName);
        songProgress = findViewById(R.id.songProgress);
        songPrevious = findViewById(R.id.songPrevious);
        songPause = findViewById(R.id.songPause);
        songNext = findViewById(R.id.songNext);
    }

    @SuppressLint("NonConstantResourceId")
    public void initialiseBottomNavigator(){
        bottomNavigation.setSelectedItemId(R.id.sport);
        bottomNavigation.setOnItemSelectedListener(item -> {
            switch (item.getItemId()){
                case R.id.save:
                    startActivity(new Intent(getApplicationContext(), SaveActivity.class));
                    return true;

                case R.id.sleep:
                    startActivity(new Intent(getApplicationContext(), SleepActivity.class));
                    return true;

                case R.id.music:
                    startActivity(new Intent(getApplicationContext(), MusicActivity.class));
                    return true;

                case R.id.sport:
                    return true;

                case R.id.info:
                    startActivity(new Intent(getApplicationContext(), InfoActivity.class));
                    return true;
            }
            return false;
        });
    }

    public void initialiseLayout(){
        initialiseTabLayout();
        initialiseFragmentAdapter();
        initialiseViewPager();
    }

    public void initialiseTabLayout(){
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
    }

    public void initialiseFragmentAdapter(){
        sportFragmentAdapter = new SportFragmentAdapter(getSupportFragmentManager(), getLifecycle());
        sportFragmentAdapter.addFragment(new SportListFragment());
        sportFragmentAdapter.addFragment(new SportChartFragment());
        sportFragmentAdapter.addFragment(new SportCalendarFragment());
        sportFragmentAdapter.addFragment(new SportTypeFragment());
        sportFragmentAdapter.addFragment(new SportStatisticsFragment());
    }

    public void initialiseViewPager(){
        viewPager2.setAdapter(sportFragmentAdapter);
        viewPager2.registerOnPageChangeCallback(new ViewPager2.OnPageChangeCallback() {
            @Override
            public void onPageSelected(int position) {
                tabLayout.selectTab(tabLayout.getTabAt(position));
            }
        });
    }

    public void initialiseMusicPlayer(){
        initialiseSongController();
        initialiseImageButtons();
        initialiseLiveData();
    }

    public void initialiseSongController(){
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
        songPrevious.setOnClickListener(v -> musicPlayer.previousButton());
        songPause.setOnClickListener(v -> musicPlayer.playButton());
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
        bottomNavigation.setSelectedItemId(R.id.sport);
    }
}