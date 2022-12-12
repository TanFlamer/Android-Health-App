package com.example.myapp.mainActivities;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.os.Bundle;
import android.widget.ImageButton;
import android.widget.SeekBar;
import android.widget.TextView;

import androidx.appcompat.app.AppCompatActivity;
import androidx.viewpager2.widget.ViewPager2;

import com.example.myapp.FragmentAdapter;
import com.example.myapp.MainApplication;
import com.example.myapp.MusicPlayer;
import com.example.myapp.R;
import com.example.myapp.fragments.music.musicList.MusicListFragment;
import com.example.myapp.fragments.music.musicPlaylists.MusicPlaylistsFragment;
import com.example.myapp.fragments.music.musicStatistics.MusicStatisticsFragment;
import com.google.android.material.bottomnavigation.BottomNavigationView;
import com.google.android.material.tabs.TabLayout;

public class MusicActivity extends AppCompatActivity {

    MainApplication mainApplication;
    FragmentAdapter musicFragmentAdapter;
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
        tabLayout = findViewById(R.id.layoutMusic);
        viewPager2 = findViewById(R.id.viewpagerMusic);
        songName = findViewById(R.id.songName);
        songProgress = findViewById(R.id.songProgress);
        songPrevious = findViewById(R.id.songPrevious);
        songPause = findViewById(R.id.songPause);
        songNext = findViewById(R.id.songNext);
    }

    //initialise bottom navigator
    @SuppressLint("NonConstantResourceId")
    public void initialiseBottomNavigator(){
        //set current item to music icon
        bottomNavigation.setSelectedItemId(R.id.music);
        //set bottom navigator listener
        bottomNavigation.setOnItemSelectedListener(item -> {
            //get intent to other activity
            Intent intent = mainApplication.getIntent(item.getItemId(), R.id.music);
            //if different activity, then start activity else stay in same activity
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

    //initialise music fragments
    public void initialiseFragmentAdapter(){
        //initialise music fragment adapter
        musicFragmentAdapter = new FragmentAdapter(getSupportFragmentManager(), getLifecycle());
        //add music list fragment
        musicFragmentAdapter.addFragment(new MusicListFragment());
        //add music playlists fragment
        musicFragmentAdapter.addFragment(new MusicPlaylistsFragment());
        //add music statistics fragment
        musicFragmentAdapter.addFragment(new MusicStatisticsFragment());
    }

    //initialise view pager
    public void initialiseViewPager(){
        //set view pager with music adapter
        viewPager2.setAdapter(musicFragmentAdapter);
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
        //set bottom navigator to music icon on resume
        bottomNavigation.setSelectedItemId(R.id.music);
        //reset song name and progress if music player uninitialised
        musicPlayer.resetMusic(songName, songProgress);
    }
}