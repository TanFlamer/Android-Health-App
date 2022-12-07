package com.example.myapp.mainActivities;

import androidx.annotation.NonNull;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;
import androidx.lifecycle.Lifecycle;
import androidx.viewpager2.adapter.FragmentStateAdapter;

import java.util.ArrayList;

public class FragmentAdapter extends FragmentStateAdapter {

    //List of fragments
    private final ArrayList<Fragment> fragmentArrayList = new ArrayList<>();

    //constructor for fragment adapter
    public FragmentAdapter(@NonNull FragmentManager fragmentManager, @NonNull Lifecycle lifecycle) {
        super(fragmentManager, lifecycle);
    }

    @NonNull
    @Override //return fragment
    public Fragment createFragment(int position) {
        return fragmentArrayList.get(position);
    }

    @Override //get fragment count
    public int getItemCount() {
        return fragmentArrayList.size();
    }

    //add new fragment
    public void addFragment(Fragment fragment){
        fragmentArrayList.add(fragment);
    }
}
