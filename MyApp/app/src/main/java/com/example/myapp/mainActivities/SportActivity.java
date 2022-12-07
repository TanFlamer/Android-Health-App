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
import com.example.myapp.fragments.sport.sportCalendar.SportCalendarFragment;
import com.example.myapp.fragments.sport.sportChart.SportChartFragment;
import com.example.myapp.fragments.sport.sportList.SportListFragment;
import com.example.myapp.fragments.sport.sportStatistics.SportStatisticsFragment;
import com.example.myapp.fragments.sport.sportType.SportTypeFragment;
import com.google.android.material.bottomnavigation.BottomNavigationView;
import com.google.android.material.tabs.TabLayout;

public class SportActivity extends AppCompatActivity {

    MainApplication mainApplication;
    FragmentAdapter sportFragmentAdapter;
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
        mainApplication = (MainApplication) getApplication();
        //initialise all components
        initialiseAll();
    }

    //initialise all components
    public void initialiseAll(){
        //link all components with ID
        initialiseViewByID();
        //initialise bottom navigator
        initialiseBottomNavigator();
        //initialise tab layout and fragments
        initialiseLayout();
        //initialise music player
        initialiseMusicPlayer();
    }

    //link all components with ID
    public void initialiseViewByID(){
        musicPlayer = mainApplication.getMusicPlayer();
        bottomNavigation = findViewById(R.id.bottom_navigator);
        tabLayout = findViewById(R.id.layoutSport);
        viewPager2 = findViewById(R.id.viewpagerSport);
        songName = findViewById(R.id.songName);
        songProgress = findViewById(R.id.songProgress);
        songPrevious = findViewById(R.id.songPrevious);
        songPause = findViewById(R.id.songPause);
        songNext = findViewById(R.id.songNext);
    }

    //initialise bottom navigator
    @SuppressLint("NonConstantResourceId")
    public void initialiseBottomNavigator(){
        //set current item to sport icon
        bottomNavigation.setSelectedItemId(R.id.sport);
        //set bottom navigator listener
        bottomNavigation.setOnItemSelectedListener(item -> {
            Intent intent = mainApplication.getIntent(item.getItemId(), R.id.sport);
            if(intent != null) startActivity(intent);
            return true;
        });
    }

    //initialise tab layout and fragments
    public void initialiseLayout(){
        //initialise tab layout
        initialiseTabLayout();
        //initialise sport fragments
        initialiseFragmentAdapter();
        //initialise view pager
        initialiseViewPager();
    }

    //initialise tab layout
    public void initialiseTabLayout(){
        //initialise tab layout listener
        tabLayout.addOnTabSelectedListener(new TabLayout.OnTabSelectedListener() {
            @Override
            public void onTabSelected(TabLayout.Tab tab) {
                //set view pager to current tab when selected
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

    //initialise sport fragments
    public void initialiseFragmentAdapter(){
        //initialise sport fragment adapter
        sportFragmentAdapter = new FragmentAdapter(getSupportFragmentManager(), getLifecycle());
        //add sport list fragment
        sportFragmentAdapter.addFragment(new SportListFragment());
        //add sport chart fragment
        sportFragmentAdapter.addFragment(new SportChartFragment());
        //add sport calendar fragment
        sportFragmentAdapter.addFragment(new SportCalendarFragment());
        //add sport type fragment
        sportFragmentAdapter.addFragment(new SportTypeFragment());
        //add sport statistics fragment
        sportFragmentAdapter.addFragment(new SportStatisticsFragment());
    }

    //initialise view pager
    public void initialiseViewPager(){
        //set view pager with sport adapter
        viewPager2.setAdapter(sportFragmentAdapter);
        //set view pager callback
        viewPager2.registerOnPageChangeCallback(new ViewPager2.OnPageChangeCallback() {
            @Override
            public void onPageSelected(int position) {
                tabLayout.selectTab(tabLayout.getTabAt(position));
            }
        });
    }

    //initialise music player
    public void initialiseMusicPlayer(){
        //initialise song seek bar
        musicPlayer.initialiseSongController(songProgress);
        //initialise song buttons
        musicPlayer.initialiseImageButtons(songPrevious, songPause, songNext);
        //initialise song name and progress live data
        musicPlayer.initialiseSongProgress(songName, songProgress);
    }

    @Override
    protected void onResume() {
        super.onResume();
        //set bottom navigator to sport icon on resume
        bottomNavigation.setSelectedItemId(R.id.sport);
        //reset song name and progress if music player uninitialised
        musicPlayer.resetMusic(songName, songProgress);
    }
}