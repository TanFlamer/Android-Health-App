package com.example.myappv2.mainfragments;

import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.viewpager2.widget.ViewPager2;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import com.example.myappv2.R;
import com.example.myappv2.subfragments.MusicAdapter;
import com.example.myappv2.subfragments.SleepAdapter;
import com.example.myappv2.subfragments.music.MusicStatistics;
import com.example.myappv2.subfragments.music.Playlists;
import com.example.myappv2.subfragments.music.Songs;
import com.example.myappv2.subfragments.sleep.SleepCalendar;
import com.example.myappv2.subfragments.sleep.SleepList;
import com.example.myappv2.subfragments.sleep.SleepStatistics;
import com.google.android.material.tabs.TabLayout;

/**
 * A simple {@link Fragment} subclass.
 * Use the {@link Sleep#newInstance} factory method to
 * create an instance of this fragment.
 */
public class Sleep extends Fragment {

    // TODO: Rename parameter arguments, choose names that match
    // the fragment initialization parameters, e.g. ARG_ITEM_NUMBER
    private static final String ARG_PARAM1 = "param1";
    private static final String ARG_PARAM2 = "param2";

    // TODO: Rename and change types of parameters
    private String mParam1;
    private String mParam2;

    public Sleep() {
        // Required empty public constructor
    }

    /**
     * Use this factory method to create a new instance of
     * this fragment using the provided parameters.
     *
     * @param param1 Parameter 1.
     * @param param2 Parameter 2.
     * @return A new instance of fragment SleepFragment.
     */
    // TODO: Rename and change types and number of parameters
    public static Sleep newInstance(String param1, String param2) {
        Sleep fragment = new Sleep();
        Bundle args = new Bundle();
        args.putString(ARG_PARAM1, param1);
        args.putString(ARG_PARAM2, param2);
        fragment.setArguments(args);
        return fragment;
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (getArguments() != null) {
            mParam1 = getArguments().getString(ARG_PARAM1);
            mParam2 = getArguments().getString(ARG_PARAM2);
        }
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        return inflater.inflate(R.layout.fragment_sleep, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        ViewPager2 viewPager2 = requireView().findViewById(R.id.viewpagerSleep);
        SleepAdapter sleepAdapter = new SleepAdapter(getChildFragmentManager(), getLifecycle());

        sleepAdapter.addFragment(new SleepList());
        sleepAdapter.addFragment(new SleepCalendar());
        sleepAdapter.addFragment(new SleepStatistics());
        viewPager2.setAdapter(sleepAdapter);

        TabLayout tabLayout = requireView().findViewById(R.id.layoutSleep);
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
}